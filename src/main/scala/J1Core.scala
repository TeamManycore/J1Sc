/*
 * Author: Steffen Reith (Steffen.Reith@hs-rm.de)
 *
 * Create Date:    Tue Sep 20 15:07:10 CEST 2016
 * Module Name:    J1Core - CPU core (ALU, Decoder, Stacks, etc)
 * Project Name:   J1Sc - A simple J1 implementation in Scala using Spinal HDL
 *
 */
import spinal.core.{Bits, _}

class J1Core(cfg : J1Config) extends Component {

  // Check the generic parameters
  assert(cfg.wordSize == 32, message = "ERROR: Wordsize should be 32!")
  assert((cfg.wordSize - 19) >= cfg.adrWidth, message = "ERROR: The width of an address is too large")

  // Internally used signals
  val internal = new Bundle {

    // Signals for memory and io port
    val memWriteMode = out Bool
    val ioWriteMode  = out Bool
    val ioReadMode   = out Bool
    val extAdr       = out UInt(cfg.wordSize bits)
    val extToWrite   = out Bits(cfg.wordSize bits)
    val toRead       = in Bits(cfg.wordSize bits)

    // Signal to stall the CPU
    val stall = in Bool

    // Interface for the interrupt system
    val irq    = in Bool
    val intVec = in Bits (cfg.adrWidth bits)

    // I/O port for instructions
    val nextInstrAdr = out (UInt(cfg.adrWidth bits))
    val memInstr     = in (Bits(cfg.wordSize bits))

  }.setName("")

  // Synchron reset
  val clrActive = ClockDomain.current.isResetActive

  // Program counter (note that the MSB is used to control dstack and rstack, hence make is one bit larger)
  val pcN = UInt(cfg.adrWidth + 1 bits)
  val pc = RegNextWhen(pcN, !(clrActive || internal.stall)) init(cfg.startAddress)
  val pcPlusOne = pc + 1

  // Instruction to be executed (insert a call-instruction for handling an interrupt)
  // 4b Instruction, 8b routing, 7 leftover bits, 13b address
  val instr = Mux(internal.irq, B"b0100_0000_0000_0000_000" ## internal.intVec.resize(cfg.wordSize - 19), internal.memInstr)

  // Data stack pointer (set to first entry, which can be arbitrary)
  val dStackPtrN = UInt(cfg.dataStackIdxWidth bits)
  val dStackPtr = RegNext(dStackPtrN) init(0)

  // Write enable signal for data stack
  val dStackWrite = Bool

  // Write enable for return stack
  val rStackWrite = Bool

  // Top of stack and next value
  val dtosN = Bits(cfg.wordSize bits)
  val dtos = RegNext(dtosN) init(0)

  // Data stack with read and write port
  val dStack = Mem(Bits(cfg.wordSize bits), wordCount = 1 << (cfg.dataStackIdxWidth))
  dStack.write(enable  = dStackWrite,
               address = dStackPtrN,
               data    = dtos)
  val dnos = dStack.readAsync(address = dStackPtr, readUnderWrite = writeFirst)

  // Check for interrupt mode, because afterwards the current instruction has to be executed
  val retPC = Mux(internal.irq, pc.asBits, pcPlusOne.asBits)

  // Set next value for RTOS (check call / interrupt or T -> R ALU instruction) (Check 12th bit (0-indexed))
  val rtosN = Mux(!instr(instr.high - 19 + 1), (retPC ## B"b0").resized, dtos)

  // Return stack pointer, set to first entry (can be arbitrary) s.t. the first write takes place at index 0
  val rStackPtrN = UInt(cfg.returnStackIdxWidth bits)
  val rStackPtr = RegNext(rStackPtrN) init(0)

  // Return stack with read and write port
  val rStack = Mem(Bits(cfg.wordSize bits), wordCount = (1 << cfg.returnStackIdxWidth))
  rStack.write(enable  = rStackWrite,
               address = rStackPtrN,
               data    = rtosN)
  val rtos = rStack.readAsync(address = rStackPtr, readUnderWrite = writeFirst)

  // Calculate difference (- dtos + dnos) and sign to be reused multiple times
  // TODO: Why +1?
  val difference = dnos.resize(cfg.wordSize + 1).asSInt - dtos.resize(cfg.wordSize + 1).asSInt
  val nosIsLess = (dtos.msb ^ dnos.msb) ? dnos.msb | difference.msb

  // Slice the ALU code out of the instruction
  // TODO: probably fine?
  val aluOp = instr((instr.high - 20) downto ((instr.high - 24) + 1))

  // Calculate the ALU result (mux all possible cases)
  val aluResult = aluOp.mux(B"0000" -> dtos,
                            B"0001" -> dnos,

                            // Arithmetic and logical operations
                            B"0010" -> (dtos.asUInt + dnos.asUInt).asBits,
                            B"1100" -> difference.resize(cfg.wordSize).asBits,
                            B"0011" -> (dtos & dnos),
                            B"0100" -> (dtos | dnos),
                            B"0101" -> (dtos ^ dnos),
                            B"0110" -> (~dtos),
                            B"1001" -> (dtos(dtos.high) ## dtos(dtos.high downto 1).asUInt),
                            B"1010" -> (dtos(dtos.high - 1 downto 0) ## B"b0"),

                            // Push rtos to dtos
                            B"1011" -> rtos,

                            // Compare operations (equal, dtos > dnos, signed and unsigned)
                            B"0111" -> B(cfg.wordSize bits, default -> (difference === 0)),
                            B"1000" -> B(cfg.wordSize bits, default -> nosIsLess),
                            B"1111" -> B(cfg.wordSize bits, default -> difference.msb),

                            // Memory / IO read operations
                            B"1101" -> internal.toRead,

                            // Misc operations (depth of dstack)
                            B"1110" -> dStackPtr.resize(cfg.wordSize bits).asBits)

  // Instruction decoder
  switch(pc.msb ## instr(instr.high downto (instr.high - 4) + 1)) {

    // If there is a high call then push the instruction (== memory access) to the data stack
    is(M"1_----") {dtosN := instr}

    // Literal instruction (Push value)
    is(M"0_1000") {dtosN := instr(instr.high - 16 downto 0).resized}

    // Jump and call instruction (do not change dtos)
    is(M"0_0000", M"0_0100") {dtosN := dtos}

    // Conditional jump (pop a 0 at dtos by adjusting the dstack pointer)
    is(M"0_0010") {dtosN := dnos}

    // Check for ALU operation
    is(M"0_011-") {dtosN := aluResult}

    // Set all bits of top of stack to true by default
    default {dtosN := (default -> True)}

  }

  // Internal ALU condition flags
  val funcTtoN     = (instr(6 downto 4).asUInt === 1) // Copy DTOS to DNOS
  val funcTtoR     = (instr(6 downto 4).asUInt === 2) // Copy DTOS to return stack
  val funcWriteMem = (instr(6 downto 4).asUInt === 3) // Write to RAM
  val funcWriteIO  = (instr(6 downto 4).asUInt === 4) // I/O write operation
  val funcReadIO   = (instr(6 downto 4).asUInt === 5) // I/O read operation
  // TODO: Probably fine
  val isALU        = !pc.msb && (instr(instr.high downto (instr.high - 3) + 1) === B"b011") // ALU operation

  // Signals for handling external memory
  internal.memWriteMode := !clrActive && isALU && funcWriteMem
  internal.ioWriteMode := !clrActive && isALU && funcWriteIO
  internal.ioReadMode := !clrActive && isALU && funcReadIO
  internal.extAdr := dtosN.asUInt
  internal.extToWrite := dnos

  // Increment for data stack pointer
  val dStackPtrInc = SInt(cfg.dataStackIdxWidth bits)

  // Handle update of data stack
  switch(pc.msb ## instr(instr.high downto (instr.high - 4) + 1)) {

    // For a high call push the instruction (== memory access) and for a literal push the value to the data stack
    is(M"1_----", M"0_1---") {dStackWrite := True; dStackPtrInc := 1}

    // Conditional jump (pop DTOS from data stack)
    is(M"0_0010") {dStackWrite := False; dStackPtrInc := -1}

    // ALU instruction (check for a possible push of data, ISA bug can be fixed by '| (instr(1 downto 0) === B"b01")')
    is(M"0_011-"){dStackWrite  := funcTtoN; dStackPtrInc := instr(1 downto 0).asSInt.resized}

    // Don't change the data stack by default
    default {dStackWrite := False; dStackPtrInc := 0}

  }

  // Update the data stack pointer
  dStackPtrN := (dStackPtr.asSInt + dStackPtrInc).asUInt

  // Increment for return stack pointer
  val rStackPtrInc = SInt(cfg.returnStackIdxWidth bits)

  // Handle the update of the return stack
  switch(pc.msb ## instr(instr.high downto (instr.high - 4) + 1)) {

    // When we do a high call (the msb of the PC is set) do a pop of return address
    is(M"1_----") {rStackWrite := False; rStackPtrInc := -1}

    // Call instruction or interrupt (push return address to stack)
    is(M"0_0100") {rStackWrite := True; rStackPtrInc := 1}

    // Conditional jump (maybe we have to push)
    is(M"0_011-") {rStackWrite := funcTtoR; rStackPtrInc := instr(3 downto 2).asSInt.resized}

    // Don't change the return stack by default
    default {rStackWrite := False; rStackPtrInc := 0}

  }

  // Update the return stack pointer
  rStackPtrN := (rStackPtr.asSInt + rStackPtrInc).asUInt

  // Handle the PC (remember cfg.adrWidth - 1 is the high indicator and instr(7) is the R -> PC field)
  switch(clrActive ## pc.msb ## instr(instr.high downto (instr.high - 4) + 1) ## instr(7) ## dtos.orR) {

    // Check if we are in reset state
    is(M"1_-_----_-_-") {pcN := cfg.startAddress}

    // Check for jump, call instruction or conditional jump
    is(M"0_0_0000_-_-", M"0_0_0100_-_-", M"0_0_0010_-_0") {pcN := instr(cfg.adrWidth downto 0).asUInt}

    // Check either for a high call or R -> PC field of an ALU instruction and load PC from return stack
    is(M"0_1_----_-_-", M"0_0_0110_1_-") {pcN := rtos(cfg.adrWidth + 1 downto 1).asUInt}

    // By default goto next instruction
    default {pcN := pcPlusOne}

  }

  // Use next PC as address of instruction memory (do not use the MSB)
  internal.nextInstrAdr := pcN(pcN.high - 1 downto 0)

}
