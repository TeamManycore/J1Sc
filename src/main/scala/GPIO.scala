/*
 * Author: Steffen Reith (Steffen.Reith@hs-rm.de)
 *
 * Creation Date:  Sun Mar 12 00:53:59 GMT+1 2017
 * Module Name:    PMod - An interface to a Digilent PMOD interface
 * Project Name:   J1Sc - A simple J1 implementation in Scala using Spinal HDL
 *
 */
import spinal.core._
import spinal.lib.bus.misc.BusSlaveFactory

class GPIO(gpioCfg : GPIOConfig) extends Component {

  // Check for the typical PMOD width
  if (gpioCfg.width != 8) {

    // Give a warning
    println("[J1Sc] WARNING: A PMod according to the digilent specification has width 8!")

  }

  // Signal used for the internal bus
  val bus = new Bundle {

    // Ports used for the direction register
    val dirEnable  = in Bool
    val dirValue   = in Bits (gpioCfg.width bits)

    // Ports used the the data register
    val dataEnable = in Bool
    val dataValue  = in Bits (gpioCfg.width bits)

  }.setName("")

  // Physically connected signals
  val io = new Bundle {

    val directions = out Bits(gpioCfg.width bits)
    val dataIn     = in Bits(gpioCfg.width bits)
    val dataOut    = out Bits(gpioCfg.width bits)

  }.setName("")

  // Register for holding the direction of the GPIO register
  val dirReg = RegNextWhen(bus.dirValue, bus.dirEnable) init (0)

  // Propagate the contents of the direction register to the interface
  io.directions := dirReg

  // Register for holding the IO data
  val dataRegN = Bits(gpioCfg.width bits)
  val dataReg = RegNext(dataRegN) init(0)

  // Check if the register was addressed by a bus transfer
  when (bus.dataEnable) {

    // Select and update the bits to be read / written
    dataRegN := (io.dataIn & (~io.directions)) | (bus.dataValue & io.directions)

  }.otherwise {

    // Update only bits to be read
    dataRegN := dataReg | (io.dataIn & (~io.directions))

  }

  // Propagate the contents of the data register to the interface
  io.dataOut := dataReg

  // Implement the bus interface
  def driveFrom(busCtrl : BusSlaveFactory, baseAddress : BigInt) : Area = new Area {

    // The direction register is mapped at address 0 and is of type r/w
    busCtrl.read(io.directions, baseAddress + 0, bitOffset = 0)
    busCtrl.nonStopWrite(bus.dirValue, bitOffset = 0) // contents of direction register will be constantly driven by the bus
    bus.dirEnable := busCtrl.isWriting(baseAddress + 0)

    // The data register is mapped at address 4 and is of type r/w
    busCtrl.read(io.dataOut, baseAddress + 4, bitOffset = 0)
    busCtrl.nonStopWrite(bus.dataValue, bitOffset = 0) // contents of direction register will be constantly driven by the bus
    bus.dataEnable := busCtrl.isWriting(baseAddress + 4)

  }

}
