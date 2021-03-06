/*
 * Author: Steffen Reith (Steffen.Reith@hs-rm.de) 
 *
 * Creation Date:  Fri Nov 25 13:04:23 GMT+1 2016
 * Module Name:    InterruptCtrl - A small interrupt controller for the J1 CPU
 * Project Name:   J1Sc - A simple J1 implementation in Scala using Spinal HDL
 *
 */
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

class InterruptCtrl(cfg : J1Config) extends Component {

  // Check the number of interrupts
  assert(isPow2(cfg.irqConfig.numOfInterrupts), "WARNING: Specify a power of 2 as number of interrupts")

  // Physical interrupt request signals
  val io = new Bundle {

    val irqReqs = in Bits (cfg.irqConfig.numOfInterrupts bits)

  }.setName("")

  // Signals used for the internal bus
  val bus = new Bundle {

    val enableWriteNewMask = in Bool
    val enableWriteIrqVec  = in Bits (cfg.irqConfig.numOfInterrupts bits)

    val irqSetData = in Bits (cfg.wordSize bits)

    val irqGetMask = out Bits (cfg.wordSize bits)
    val irqVectors = out Vec(Bits(cfg.wordSize bits), cfg.irqConfig.numOfInterrupts)

  }.setName("")

  // Internally used signals
  val internal = new Bundle {

    val intVec = out Bits(cfg.wordSize bits)
    val irq    = out Bool

  }.setName("")

  // Register the irq mask (disable all interrupts after reset)
  val irqMask = RegNextWhen(bus.irqSetData.resize(cfg.irqConfig.numOfInterrupts),
                            bus.enableWriteNewMask,
                            B(cfg.irqConfig.numOfInterrupts bits, default -> False))

  // Generate an output version of the current mask
  bus.irqGetMask := irqMask.resize(cfg.wordSize)

  // Enable signals for the interrupt vectors
  val irqVecWriteEnable = Bits(cfg.irqConfig.numOfInterrupts bits)
  irqVecWriteEnable := bus.enableWriteIrqVec.resize(cfg.irqConfig.numOfInterrupts)

  // Create a register file for storing the interrupt vectors
  val irqVectors = Vec(for(i <- 0 until cfg.irqConfig.numOfInterrupts) yield {

    // Create the ith register and truncate data read from the bus
    RegNextWhen((bus.irqSetData >> 1).resize(cfg.adrWidth),
                irqVecWriteEnable(i),
                B(cfg.adrWidth bits, default -> False))

  })

  // Wire all interrupt vectors to an IO-port
  (bus.irqVectors, irqVectors).zipped.foreach(_ := _.resize(cfg.wordSize))

  // All interrupts are asynchronous, hence make them synchron
  val irqSync = BufferCC(io.irqReqs,
                         init = B(0, cfg.irqConfig.numOfInterrupts bits),
                         bufferDepth = cfg.irqConfig.irqLatency)

  // Check all interrupts with priority from 0 (high) to noOfInterrupts - 1 (low)
  val intNo = OHToUInt(OHMasking.first(irqSync))

  // Provide the corresponding interrupt vector
  internal.intVec := irqVectors(intNo).resize(cfg.wordSize)

  // Generate a rising edge when an interrupt has happened (init value is false)
  internal.irq := (irqSync & irqMask).orR.rise(False)

  // Implement the bus interface
  def driveFrom(busCtrl : BusSlaveFactory, baseAddress : BigInt) = new Area {

    // A read port for the interrupt mask
    busCtrl.read(bus.irqGetMask, baseAddress + cfg.irqConfig.numOfInterrupts, 0)

    // The enable signal is constantly driven by the data of the memory bus
    busCtrl.nonStopWrite(bus.irqSetData, 0)

    // Generate the write enable signal for the interrupt mask
    bus.enableWriteNewMask := busCtrl.isWriting(baseAddress + cfg.irqConfig.numOfInterrupts)

    // r/w-registers for all irq-vectors
    for (i <- 0 until cfg.irqConfig.numOfInterrupts) {

      // A r/w register access for the ith interrupt vector
      busCtrl.read(bus.irqVectors(i), baseAddress + i, 0)

      // Generate the write enable signal for the ith interrupt vector
      bus.enableWriteIrqVec(i) := busCtrl.isWriting(baseAddress + i)

    }

  }

}
