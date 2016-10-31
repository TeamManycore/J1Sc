/*
 * Author: Steffen Reith (Steffen.Reith@hs-rm.de)
 * Committer: Steffen Reith
 *
 * Create Date:    Tue Sep 27 10:35:21 CEST 2016 
 * Module Name:    J1Core - Toplevel
 * Project Name:   J1Sc - A simple J1 implementation in scala
 *
 * Hash: ece2c62952a1dd372f706f2c3f2c22e368410ec9
 * Date: Mon Oct 10 19:37:29 2016 +0200
 */
import spinal.core._
import spinal.lib._

class J1(wordSize            : Int = 16,
         dataStackIdxWidth   : Int =  8,
         returnStackIdxWidth : Int =  6,
         addrWidth           : Int = 13,
         startAddress        : Int =  0) extends Component {

  // I/O ports
  val io = new Bundle {

    // I/O signals for memory data port
    val writeEnable = out Bool
    val dataAddress  = out UInt(addrWidth bits)
    val dataWrite = out Bits(wordSize bits)
    val dataRead = in Bits(wordSize bits)

  }.setName("")

  // Signals for main memory
  val memWriteEnable = Bool
  val memAdr = UInt(addrWidth bits)
  val memWrite = Bits(wordSize bits)
  val memRead = Bits(wordSize bits)

  // Create main memory
  def content = List(B"1000_0000_0000_0111", //  0. Push 7
                     B"1000_0000_0000_0011", //  1. Push 3
                     B"0000_0000_0000_0100", //  2. Jump 4
                     B"1000_0000_0000_0001", //  3. Push 1
                     B"1000_0000_0000_1111", //  4. Push 15
                     B"0110_0010_0000_0011", //  5. Add, drop and push
                     B"0110_0010_0000_0001", //  6. Add and push
                     B"0110_0010_0000_0000", //  7. Add and no push
                     B"0010_0000_0000_1011", //  8. Jump 11 if tos is zero
                     B"1000_0000_0000_0000", //  9. Push 0
                     B"0010_0000_0000_1010", // 10. Jump 10 if tos is zero
                     B"0100_0000_0010_0000", // 11. Call 32
                     B"1000_0000_0000_0001", // 12. Push 1
                     B"0110_0111_0000_0001", // 13. Compare tos and nos push result
                     B"1000_0000_0000_0011", // 14. Push 3
                     B"1000_0000_0000_0011", // 15. Push 3
                     B"0110_0111_0000_0001", // 16. Compare tos and nos push result
                     B"0000_0000_0001_0001", // 17. Jump 17
                     B"0110_0000_0000_0000", // 18. NOP
                     B"0110_0000_0000_0000", // 19. NOP
                     B"0110_0000_0000_0000", // 20. NOP
                     B"0110_0000_0000_0000", // 21. NOP
                     B"0110_0000_0000_0000", // 22. NOP
                     B"0110_0000_0000_0000", // 23. NOP
                     B"0110_0000_0000_0000", // 24. NOP
                     B"0110_0000_0000_0000", // 25. NOP
                     B"0110_0000_0000_0000", // 26. NOP
                     B"0110_0000_0000_0000", // 27. NOP
                     B"0110_0000_0000_0000", // 28. NOP
                     B"0110_0000_0000_0000", // 29. NOP
                     B"0110_0000_0000_0000", // 30. NOP
                     B"0110_0000_0000_0000", // 31. NOP
                     B"1000_0000_0001_0001", // 32. Push 17
                     B"1000_0000_0001_0000", // 33. Push 16
                     B"0110_1111_0000_0001", // 34. Compare unsigned and push
                     B"1011_1111_1111_1111", // 35. Push +maxint
                     B"1111_1111_1111_1111", // 36. Push -1
                     B"0110_1000_0000_0001", // 37. Compare signed and push
                     B"0110_0001_0000_0011", // 38. Pop
                     B"0110_0001_0000_0011", // 39. Pop
                     B"0110_0001_0000_0011", // 40. Pop
                     B"0110_0001_0000_0011", // 41. Pop
                     B"0110_0001_0000_0011", // 42. Pop
                     B"0110_0001_0000_0011", // 43. Pop
                     B"1000_0000_1111_1110", // 44. Push 254
                     B"1000_0001_0000_0000", // 45. Push 256
                     B"0110_0000_0011_0000", // 46. Write to external RAM
                     B"0110_0001_0000_0011", // 47. Pop
                     B"0110_0001_0000_0011", // 48. Pop
                     B"0111_0000_0000_1100") // 49. Return from Subroutine
  val mainMemory = Mem(Bits(wordSize bits),
                            content ++ List.fill((1 << addrWidth) - content.length)(B(0, wordSize bits)))

  // Create a new CPU core
  val coreJ1CPU = new J1Core(wordSize, dataStackIdxWidth, returnStackIdxWidth, addrWidth, startAddress)

  // Create data port for mainMem
  mainMemory.write(enable  = memWriteEnable,
                   address = memAdr,
                   data    = memWrite)
  memRead := mainMemory.readSync(address = memAdr, readUnderWrite = readFirst)

  // Instruction port (read only)
  coreJ1CPU.io.instr := mainMemory.readSync(address = coreJ1CPU.io.instrAdr, readUnderWrite = readFirst)

  // connect the CPU core with the internal memory
  memWriteEnable <> coreJ1CPU.io.memWriteEnable
  memAdr <> coreJ1CPU.io.extAdr
  memWrite <> coreJ1CPU.io.extToWrite
  coreJ1CPU.io.memToRead <> memRead

  // Wire the IO data bus to the outside world
  io.writeEnable <> coreJ1CPU.io.ioWriteEnable
  io.dataAddress <> coreJ1CPU.io.extAdr
  coreJ1CPU.io.ioToRead <> io.dataRead
  io.dataWrite <> coreJ1CPU.io.extToWrite

}

object J1 {

  // Make the reset synchron and use the rising edge
  val globalClockConfig = ClockDomainConfig(clockEdge        = RISING,
                                            resetKind        = SYNC,
                                            resetActiveLevel = HIGH)

  def main(args: Array[String]) {

    // Generate HDL files
    SpinalConfig(genVhdlPkg = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 targetDirectory="gen/src/vhdl").generateVhdl({

                                                                // Set name for the synchronous reset
                                                                ClockDomain.current.reset.setName("clr")
                                                                new J1(dataStackIdxWidth = 3,
                                                                          returnStackIdxWidth = 2)

                                                              }).printPruned()
    SpinalConfig(defaultConfigForClockDomains = globalClockConfig,
                 targetDirectory="gen/src/verilog").generateVerilog({

                                                                      // Set name for the synchronous reset
                                                                      ClockDomain.current.reset.setName("clr")
                                                                      new J1(dataStackIdxWidth = 3,
                                                                                returnStackIdxWidth = 2)

                                                                    }).printPruned()

  }

}