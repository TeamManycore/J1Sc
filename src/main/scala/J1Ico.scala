/*
 * Author: Steffen Reith (Steffen.Reith@hs-rm.de)
 *
 * Creation Date:  Sun Nov 19 14:08:53 GMT+1 2017 
 * Module Name:    J1SoC - A small but complete system based on the J1-core
 * Project Name:   J1Sc - A simple J1 implementation in Scala using Spinal HDL
 *
 */
import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._
import spinal.lib.io._

class J1Ico(j1Cfg    : J1Config,
            boardCfg : CoreConfig) extends Component {

  val io = new Bundle {

    // Asynchron reset
    val reset = in Bool

    // A board clock
    val boardClk       = in Bool
    val boardClkLocked = in Bool

    // Asynchronous interrupts from the outside world
    val extInt = in Bits (j1Cfg.irqConfig.numOfInterrupts - j1Cfg.irqConfig.numOfInternalInterrupts bits)

    // The physical pins for the connected LEDs
    val leds = out Bits(boardCfg.ledBankConfig.width bits)

    // The physical pins for the LEDs that support PWM
    val pwmLeds = out Bits(boardCfg.pwmConfig.numOfChannels bits)

    // The physical pins for pmod A
    val pmodA = master(TriStateArray(boardCfg.gpioConfig.width bits))

    // I/O pins for the UART
    val rx = in Bool // UART input
    val tx = out Bool // UART output

  }.setName("")

  // Physical clock area (connected to a physical clock generator (e.g. crystal oscillator))
  val clkCtrl = new Area {

    // Create a clock domain which is related to the synthesized clock
    val coreClockDomain = ClockDomain.internal("core", frequency = boardCfg.coreFrequency)

    // Connect the synthesized clock
    coreClockDomain.clock := io.boardClk

    // Connect the new asynchron reset
    coreClockDomain.reset := coreClockDomain(RegNext(ResetCtrl.asyncAssertSyncDeassert(

      // Hold the reset as long as the PLL is not locked
      input = io.reset || (! io.boardClkLocked),
      clockDomain = coreClockDomain

    )))

  }

  // Generate the application specific clocking area
  val coreArea = new ClockingArea(clkCtrl.coreClockDomain) {
    //var cpuArray = new Array[J1](9)
    var cpu = new J1(j1Cfg)
    //var peripheralBusArray  = new Array[J1Bus](9)
    //var peripheralBusCtrlArray = new Array[J1BusSlaveFactory](9)
    var routerArray = new Array[Router](9)
    var routerBridgeArrray = new Array[Area](9)

    val peripheralBus     = cpu.bus.cpuBus.delayIt(boardCfg.ioWaitStates)
    val peripheralBusCtrl = J1BusSlaveFactory(peripheralBus)

    for (i <- 0 to 8){
      //cpuArray(i) = new J1(j1Cfg)

      //peripheralBusArray(i)     = cpuArray(i).bus.cpuBus.delayIt(boardCfg.ioWaitStates)
      //peripheralBusCtrlArray(i) = J1BusSlaveFactory(peripheralBusArray(i))

      routerArray(i)        = new Router(j1Cfg, i)
    }


    //peripheralBusArray(0)     = cpu.bus.cpuBus.delayIt(boardCfg.ioWaitStates)
    //peripheralBusCtrlArray(0) = J1BusSlaveFactory(peripheralBusArray(0))
    //val router = new Router(j1Cfg, 0)
    val routerBridge = routerArray(0).driveFrom(peripheralBusCtrl, baseAddress = 0xA0)

    //routerBridgeArrray(0) = routerArray(0).driveFrom(peripheralBusCtrl, baseAddress =0xA0)


    for (i <- 0 to 8){
      // West Configuration
      if (i % 3 >= 2) {
        routerArray(i-2).io.inEast := routerArray(i).io.outWest
      } else {
        routerArray(i+1).io.inEast := routerArray(i).io.outWest
      }

      // East Configuration
      if (i % 3 <= 0) {
        routerArray(i+2).io.inWest := routerArray(i).io.outEast
      } else {
        routerArray(i-1).io.inWest := routerArray(i).io.outEast
      }

      // North Configuration
      if (i / 3 <= 0) {
        routerArray(i+6).io.inSouth := routerArray(i).io.outNorth
      } else {
        routerArray(i-3).io.inSouth := routerArray(i).io.outNorth
      }

      // South Configuration
      if (i / 3 >= 2) {
        routerArray(i-6).io.inNorth := routerArray(i).io.outSouth
      } else {
        routerArray(i+3).io.inNorth := routerArray(i).io.outSouth
      }
    }

    // Create a LED array at base address 0x40
    val ledArray  = new LEDArray(j1Cfg, boardCfg.ledBankConfig)
    val ledBridge = ledArray.driveFrom(peripheralBusCtrl, baseAddress = 0x40)

    // Connect the physical LED pins to the outside world
    io.leds := ledArray.io.leds

    // Create the PWMs fpr the RGB-leds at 0x50
    val pwm = new PWM(j1Cfg, boardCfg.pwmConfig)
    val pwmBridge = pwm.driveFrom(peripheralBusCtrl, baseAddress = 0x50)

    // Connect the pwm channels physically
    io.pwmLeds := pwm.io.pwmChannels

    // Create a PMOD at base address 0x60
    val pmodA       = new GPIO(boardCfg.gpioConfig)
    val pmodABridge = pmodA.driveFrom(peripheralBusCtrl, baseAddress = 0x70)

    // Connect the gpio register to pmodA
    io.pmodA.write       <> pmodA.io.dataOut
    pmodA.io.dataIn      <> io.pmodA.read
    io.pmodA.writeEnable <> pmodA.io.directions

    // Create two timer and map it at 0xC0 and 0xD0
    val timerA       = new Timer(j1Cfg.timerConfig)
    val timerABridge = timerA.driveFrom(peripheralBusCtrl, baseAddress = 0xC0)
    val timerB       = new Timer(j1Cfg.timerConfig)
    val timerBBridge = timerB.driveFrom(peripheralBusCtrl, baseAddress = 0xD0)

    // Create an UART interface with fixed capabilities
    val uartCtrlGenerics = UartCtrlGenerics(dataWidthMax      = boardCfg.uartConfig.dataWidthMax,
                                            clockDividerWidth = boardCfg.uartConfig.clockDividerWidth,
                                            preSamplingSize   = boardCfg.uartConfig.preSamplingSize,
                                            samplingSize      = boardCfg.uartConfig.samplingSize,
                                            postSamplingSize  = boardCfg.uartConfig.postSamplingSize)
    val uartCtrlInitConfig = UartCtrlInitConfig(baudrate   = boardCfg.uartConfig.baudrate,
                                                dataLength = boardCfg.uartConfig.dataLength,
                                                parity     = boardCfg.uartConfig.parity,
                                                stop       = boardCfg.uartConfig.stop)
    val uartMemMapConfig = UartCtrlMemoryMappedConfig(uartCtrlConfig                = uartCtrlGenerics,
                                                      initConfig                    = uartCtrlInitConfig,
                                                      busCanWriteClockDividerConfig = false,
                                                      busCanWriteFrameConfig        = false,
                                                      txFifoDepth                   = boardCfg.uartConfig.fifoDepth,
                                                      rxFifoDepth                   = boardCfg.uartConfig.fifoDepth)
    val uartCtrl = new UartCtrl(uartCtrlGenerics)

    // Map the UART to 0xF0 and enable the generation of read interrupts
    val uartBridge = uartCtrl.driveFrom(peripheralBusCtrl, uartMemMapConfig, baseAddress = 0xF0)
    uartBridge.interruptCtrl.readIntEnable := True

    // Tell Spinal that some unneeded signals are allowed to be pruned to avoid warnings
    uartBridge.interruptCtrl.interrupt.allowPruning()

    // Connect the physical UART pins to the outside world
    io.tx := uartCtrl.io.uart.txd
    uartCtrl.io.uart.rxd := io.rx

    // Create an interrupt controller, map it to 0xE0 and connect all interrupts
    val intCtrl = new InterruptCtrl(j1Cfg)
    val intCtrlBridge = intCtrl.driveFrom(peripheralBusCtrl, baseAddress = 0xE0)
    intCtrl.io.irqReqs(intCtrl.io.irqReqs.high downto j1Cfg.irqConfig.numOfInternalInterrupts) <> io.extInt
    intCtrl.io.irqReqs(0) <> uartBridge.interruptCtrl.readInt
    intCtrl.io.irqReqs(1) <> timerA.io.interrupt
    intCtrl.io.irqReqs(2) <> timerB.io.interrupt
    cpu.internal.intVec <> intCtrl.internal.intVec.resize(j1Cfg.adrWidth)
    cpu.internal.irq <> intCtrl.internal.irq

  }

}

object J1Ico {

  // Make the reset synchron and use the rising edge
  val globalClockConfig = ClockDomainConfig(clockEdge        = RISING,
                                            resetKind        = SYNC,
                                            resetActiveLevel = HIGH)

  def main(args : Array[String]) {

    def elaborate = {

      // Configuration of CPU-core
      val j1Cfg = J1Config.forth

      // Configuration of the used board
      val boardCfg = CoreConfig.icoBoard

      // Create a system instance
      new J1Ico(j1Cfg, boardCfg)

    }

    // Write a message
    println("[J1Sc] Create the J1 for an IcoBoard")

    // Generate VHDL
    SpinalConfig(mergeAsyncProcess = true,
                 genVhdlPkg = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 targetDirectory="gen/src/vhdl").generateVhdl(elaborate).printPruned()

    // Generate Verilog / Maybe mergeAsyncProcess = false helps verilator to avoid wrongly detected combinatorial loops
    SpinalConfig(mergeAsyncProcess = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 targetDirectory="gen/src/verilog").generateVerilog(elaborate).printPruned()

  }

}
