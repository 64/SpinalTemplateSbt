package projectname

import spinal.core._
import spinal.core.sim._

import spinal.lib.misc.pipeline._
import spinal.lib.Flow

class Cpu extends Component {
  val fetch, decode, execute = CtrlLink()
  val f2d = StageLink(fetch.down, decode.up)
  val d2e = StageLink(decode.down, execute.up)

  val PC = Payload(UInt(8 bits))
  val INSTRUCTION = Payload(Bits(16 bits))

  val led = out(Reg(Bits(8 bits))) init (0)

  val fetcher = new fetch.Area {
    val pcReg = Reg(PC).simPublic() init (0)
    up(PC) := pcReg
    up.valid := True
    when(up.isFiring) {
      pcReg := PC + 1
    }

    val mem = Mem.fill(256)(INSTRUCTION).simPublic
    INSTRUCTION := mem.readAsync(PC)
  }

  val decoder = new decode.Area {
    val opcode = INSTRUCTION(7 downto 0)
    val IS_ADD = insert(opcode === 0x1)
    val IS_JUMP = insert(opcode === 0x2)
    val IS_LED = insert(opcode === 0x3)
    val IS_DELAY = insert(opcode === 0x4)

    // Add a fake hazard when executing ADD after JUMP
    val anyHazard = isValid && execute.isValid && IS_ADD && execute(IS_JUMP)
    when(anyHazard) {
      haltIt()
    }
  }

  val alu = new execute.Area {
    val regfile = Reg(UInt(8 bits)) init (0)

    val flush = False
    for (stage <- List(fetch, decode)) {
      stage.throwWhen(flush, usingReady = true)
      // stage.throwWhen(flush, usingReady = (stage == fetch))
    }

    val delayCounter = Reg(UInt(8 bits)) init (0)

    val retire = Flow(UInt(8 bits)).simPublic()
    retire.valid := isValid
    retire.payload := PC

    when(isValid) {
      when(decoder.IS_ADD) {
        regfile := regfile + U(INSTRUCTION(15 downto 8))
      }
      when(decoder.IS_JUMP) {
        flush := True
        fetcher.pcReg := U(INSTRUCTION(15 downto 8))
      }
      when(decoder.IS_LED) {
        led := B(regfile)
      }
      when(decoder.IS_DELAY) {
        delayCounter := delayCounter + 1
        when(delayCounter === U(INSTRUCTION(15 downto 8))) {
          delayCounter := 0
        } otherwise {
          execute.haltIt()
        }
      }
    }
  }

  Builder(fetch, decode, execute, f2d, d2e)
}

object MyTopLevelSim extends App {
  SimConfig.withVcdWave.compile(new Cpu).doSim(seed = 2) { dut =>
    def nop() = BigInt(0)
    def add(value: Int) = BigInt(1 | (value << 8))
    def jump(target: Int) = BigInt(2 | (target << 8))
    def led() = BigInt(3)
    def delay(cycles: Int) = BigInt(4 | (cycles << 8))
    val mem = dut.fetcher.mem
    mem.setBigInt(0, nop())
    mem.setBigInt(1, nop())
    mem.setBigInt(2, nop())
    mem.setBigInt(3, jump(0x1))
    mem.setBigInt(4, add(0x4))

    SimTimeout(1000)
    dut.clockDomain.forkStimulus(10)

    def checkNextPc() = {
      dut.clockDomain.waitSamplingWhere(dut.alu.retire.valid.toBoolean)
      dut.alu.retire.payload.toInt
    }
    assert(checkNextPc() == 0)
    assert(checkNextPc() == 1)
    assert(checkNextPc() == 2)
    assert(checkNextPc() == 3)
    assert(checkNextPc() == 1)
    assert(checkNextPc() == 2)
    assert(checkNextPc() == 3)
    assert(checkNextPc() == 1)

  // dut.clockDomain.waitSampling(100)
  }
}
