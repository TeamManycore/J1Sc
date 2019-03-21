import spinal.core._
import spinal.lib.bus.misc.BusSlaveFactory

class Router (j1Cfg  : J1Config, currentCore : Int) extends Component {

  def calcSel(cur : UInt , tar : Bits) : Bits = {
    val result   = Reg(Bits(3 bits)) init (0)
    //val result = B(0,3 bits)
    val East, West, North, South = U(0, 4 bits)
    //val current = U(cur, 4 bits)
    val current = cur
    // east
    when ((tar.asUInt % 3) > (current % 3)){
      East := ((tar.asUInt % 3) - (current % 3)) % 3
    }.otherwise{
      East := ((tar.asUInt % 3) + 3 - (current % 3)) % 3
    }

    when((tar.asUInt / 3) > (current / 3)){
      North := ((tar.asUInt / 3) - (current / 3)) % 3
    }.otherwise{
      North := ((tar.asUInt / 3) + 3 - (current / 3)) % 3
    }

    West  := (3 - East ) % 3
    South := (3 - North) % 3

    when ((current % 2) === 0 && (North =/= 0) ){
      result(1) -> true
      result(0) -> (North >= South)
    }.otherwise{
      result(1) -> false
      result(0) -> (West >= East)
    }

    when (current.asBits === tar) {
      result(2) -> true
    }
    result
  }

  val io = new Bundle{
    val outNorth, outEast, outSouth, outWest, outCore = out Bits(12 bits)
    val inNorth, inEast, inSouth, inWest, inCore = in Bits(12 bits)
    val tar = out Bits(4 bits)
  }.setName("")


  val bus = new Bundle {
    val writeEnable = in Bool
    val inst        = in Bits(16 bits)
    val target      = in Bits(4 bits)
  }.setName("")

  // Register for holding the bit-vector storing the LED states
  val coreReg   = Reg(Bits(12 bits)) init (0)
  val northReg  = Reg(Bits(12 bits)) init (0)
  val southReg  = Reg(Bits(12 bits)) init (0)
  val westReg   = Reg(Bits(12 bits)) init (0)
  val eastReg   = Reg(Bits(12 bits)) init (0)
  val currentNr   = Reg(Bits(4 bits)) init (currentCore)

  val result   = Bits(3 bits)
  result.clearAll()
  //val currentNr = currentCore

  val East, West, North, South = UInt(4 bits)
  East.clearAll()
  West.clearAll()
  North.clearAll()
  South.clearAll()


  // Check for write mode
  when(bus.writeEnable) {

    when ((io.tar & B"0011").asUInt > (currentNr & B"0011").asUInt){
      East := ((io.tar & B"0011").asUInt - (currentNr & B"0011").asUInt) & U"0011"
    }.otherwise{
      East := ((io.tar & B"0011").asUInt + U"0011" - (currentNr & B"0011").asUInt) & U"0011"
    }

    when((io.tar.asUInt / U"0011") > (currentNr.asUInt / U"0011")){
      North := ((io.tar.asUInt / U"0011") - (currentNr.asUInt / U"0011")) & U"0011"
    }.otherwise{
      North := ((io.tar.asUInt / U"0011") + U"0011" - (currentNr.asUInt / U"0011")) & U"0011"
    }

    West  := ((U"0011" - East).asBits & B"0011").asUInt
    South := ((U"0011" - North).asBits & B"0011").asUInt

    when ((currentNr.asUInt % 2) === 0 && (North =/= 0) ){
      result := B"01" ## (North >= South)
    }.otherwise{
      result := B"00" ##  (West <= East)
    }

    when (currentNr === io.tar) {
      result(2) := True
    }

    // Set register value
    switch (result) {
      is(B"000"){eastReg:= bus.inst(11 downto(0))}
      is(B"011"){northReg := bus.inst(11 downto(0))}
      is(B"010"){southReg := bus.inst(11 downto(0))}
      is(B"001"){westReg := bus.inst(11 downto(0))}
      default{coreReg := bus.inst(11 downto(0))}
    }
  }

  io.tar := bus.inst(bus.inst.high downto(bus.inst.high - 3))

  io.outCore  := coreReg
  io.outNorth := northReg
  io.outSouth := southReg
  io.outEast  := eastReg
  io.outWest  := westReg

  // Implement the bus interface
  def driveFrom(busCtrl : BusSlaveFactory, baseAddress : BigInt) : Area = new Area {

    busCtrl.nonStopWrite(bus.inst, 0) // ledState will be constantly driven by the data of the memory bus
    //busCtrl.nonStopWrite(bus.target, 1) // ledState will be constantly driven by the data of the memory bus

    busCtrl.read(io.outCore,baseAddress+0)
    bus.writeEnable := busCtrl.isWriting(baseAddress + 0)


  }

}
