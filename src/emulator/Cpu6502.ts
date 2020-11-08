import Bus from "./Bus";
import Instruction from "./Instruction";

enum FLAGS6502 {
    C = (1 << 0), //carry bit
    Z = (1 << 1), //zero
    I = (1 << 2), //disable interrupts
    D = (1 << 3), //decimal mode (unused in this implementation)
    B = (1 << 4), //break
    U = (1 << 5), //unused
    V = (1 << 6), //overflow
    N = (1 << 7) //negative
}


class Cpu6502 {

    private bus: Bus | undefined;

    private IMP(): number {
        this.fetched = this.a;
        return 0;
    }

    private IMM(): number {
        this.pc++;
        this.addr_abs = this.pc;
        return 0;
    }

    private ZP0(): number {
        this.addr_abs = this.read(this.pc)
        this.pc++;
        this.addr_abs &= 0x00FF;
        return 0;
    }

    private ZPX(): number {
        this.addr_abs = this.read(this.pc) + this.x;
        this.pc++;
        this.addr_abs &= 0x00FF;
        return 0;
    }

    private ZPY(): number {
        this.addr_abs = this.read(this.pc) + this.y;
        this.pc++;
        this.addr_abs &= 0x00FF;
        return 0;
    }

    private ABS(): number {
        const lo = this.read(this.pc);
        this.pc++;
        const hi = this.read(this.pc);
        this.pc++;

        this.addr_abs = (hi << 8) | lo;
        return 0;
    }

    private ABX(): number {
        const lo = this.read(this.pc);
        this.pc++;
        const hi = this.read(this.pc);
        this.pc++;

        this.addr_abs = (hi << 8) | lo;
        this.addr_abs += this.x;
        if ((this.addr_abs & 0xFF00) !== (hi << 8)) {
            return 1;
        }
        return 0;
    }

    private ABY(): number {
        const lo = this.read(this.pc);
        this.pc++;
        const hi = this.read(this.pc);
        this.pc++;

        this.addr_abs = (hi << 8) | lo;
        this.addr_abs += this.y;
        if ((this.addr_abs & 0xFF00) !== (hi << 8)) {
            return 1;
        }
        return 0;
    }

    private IND(): number {
        const ptr_lo = this.read(this.pc);
        this.pc++;
        const ptr_hi = this.read(this.pc);
        this.pc++;

        const ptr: number = (ptr_hi << 8) | ptr_lo;

        if (ptr_lo === 0x00FF) {
            this.addr_abs = (this.read(ptr & 0xFF00) << 8) | this.read(ptr);
        } else {
            this.addr_abs = (this.read(ptr + 1) << 8) | this.read(ptr)
        }
        return 0
    }

    private IZX(): number {
        const t = this.read(this.pc)
        this.pc++;
        const lo = this.read((t + this.x) & 0x00FF)
        const hi = this.read((t + this.x + 1) & 0x00FF)
        this.addr_abs = (hi << 8) | lo;
        return 0;
    }

    private IZY(): number {
        const t = this.read(this.pc)
        this.pc++;
        const lo = this.read(t & 0x00FF)
        const hi = this.read((t + 1) & 0x00FF)
        this.addr_abs = (hi << 8) | lo;
        this.addr_abs += this.y;

        if ((this.addr_abs & 0xFF00) !== (hi << 8)) {
            return 1;
        }
        return 0;
    }

    private REL(): number {
        this.addr_rel = this.read(this.pc)
        this.pc++;
        if (this.addr_rel & 0x80)
            this.addr_rel |= 0xFF00;
        return 0;
    }

    public a: number = (0x00); //accumulator register
    public x: number = (0x00); //x register
    public y: number = (0x00); //y register
    public stkp: number = (0x00); //stack pointer (points to location on bus)
    public pc: number = (0x0000); //program counter
    public status: number = (0x00); //status register

    public fetched: number = (0x00);
    public addr_abs: number = 0x0000;
    public addr_rel: number = (0x00);
    public opcode: number = (0x00);
    public cycles: number = (0)

    public lookup: Instruction[] = [];

    constructor() {
        this.lookup = [
            new Instruction("BRK", "BRK", "IMM", 7), new Instruction("ORA", "ORA", "IZX", 6), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("???", "NOP", "IMP", 3), new Instruction("ORA", "ORA", "ZP0", 3), new Instruction("ASL", "ASL", "ZP0", 5), new Instruction("???", "XXX", "IMP", 5), new Instruction("PHP", "PHP", "IMP", 3), new Instruction("ORA", "ORA", "IMM", 2), new Instruction("ASL", "ASL", "IMP", 2), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "NOP", "IMP", 4), new Instruction("ORA", "ORA", "ABS", 4), new Instruction("ASL", "ASL", "ABS", 6), new Instruction("???", "XXX", "IMP", 6),
            new Instruction("BPL", "BPL", "REL", 2), new Instruction("ORA", "ORA", "IZY", 5), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("???", "NOP", "IMP", 4), new Instruction("ORA", "ORA", "ZPX", 4), new Instruction("ASL", "ASL", "ZPX", 6), new Instruction("???", "XXX", "IMP", 6), new Instruction("CLC", "CLC", "IMP", 2), new Instruction("ORA", "ORA", "ABY", 4), new Instruction("???", "NOP", "IMP", 2), new Instruction("???", "XXX", "IMP", 7), new Instruction("???", "NOP", "IMP", 4), new Instruction("ORA", "ORA", "ABX", 4), new Instruction("ASL", "ASL", "ABX", 7), new Instruction("???", "XXX", "IMP", 7),
            new Instruction("JSR", "JSR", "ABS", 6), new Instruction("AND", "AND", "IZX", 6), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("BIT", "BIT", "ZP0", 3), new Instruction("AND", "AND", "ZP0", 3), new Instruction("ROL", "ROL", "ZP0", 5), new Instruction("???", "XXX", "IMP", 5), new Instruction("PLP", "PLP", "IMP", 4), new Instruction("AND", "AND", "IMM", 2), new Instruction("ROL", "ROL", "IMP", 2), new Instruction("???", "XXX", "IMP", 2), new Instruction("BIT", "BIT", "ABS", 4), new Instruction("AND", "AND", "ABS", 4), new Instruction("ROL", "ROL", "ABS", 6), new Instruction("???", "XXX", "IMP", 6),
            new Instruction("BMI", "BMI", "REL", 2), new Instruction("AND", "AND", "IZY", 5), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("???", "NOP", "IMP", 4), new Instruction("AND", "AND", "ZPX", 4), new Instruction("ROL", "ROL", "ZPX", 6), new Instruction("???", "XXX", "IMP", 6), new Instruction("SEC", "SEC", "IMP", 2), new Instruction("AND", "AND", "ABY", 4), new Instruction("???", "NOP", "IMP", 2), new Instruction("???", "XXX", "IMP", 7), new Instruction("???", "NOP", "IMP", 4), new Instruction("AND", "AND", "ABX", 4), new Instruction("ROL", "ROL", "ABX", 7), new Instruction("???", "XXX", "IMP", 7),
            new Instruction("RTI", "RTI", "IMP", 6), new Instruction("EOR", "EOR", "IZX", 6), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("???", "NOP", "IMP", 3), new Instruction("EOR", "EOR", "ZP0", 3), new Instruction("LSR", "LSR", "ZP0", 5), new Instruction("???", "XXX", "IMP", 5), new Instruction("PHA", "PHA", "IMP", 3), new Instruction("EOR", "EOR", "IMM", 2), new Instruction("LSR", "LSR", "IMP", 2), new Instruction("???", "XXX", "IMP", 2), new Instruction("JMP", "JMP", "ABS", 3), new Instruction("EOR", "EOR", "ABS", 4), new Instruction("LSR", "LSR", "ABS", 6), new Instruction("???", "XXX", "IMP", 6),
            new Instruction("BVC", "BVC", "REL", 2), new Instruction("EOR", "EOR", "IZY", 5), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("???", "NOP", "IMP", 4), new Instruction("EOR", "EOR", "ZPX", 4), new Instruction("LSR", "LSR", "ZPX", 6), new Instruction("???", "XXX", "IMP", 6), new Instruction("CLI", "CLI", "IMP", 2), new Instruction("EOR", "EOR", "ABY", 4), new Instruction("???", "NOP", "IMP", 2), new Instruction("???", "XXX", "IMP", 7), new Instruction("???", "NOP", "IMP", 4), new Instruction("EOR", "EOR", "ABX", 4), new Instruction("LSR", "LSR", "ABX", 7), new Instruction("???", "XXX", "IMP", 7),
            new Instruction("RTS", "RTS", "IMP", 6), new Instruction("ADC", "ADC", "IZX", 6), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("???", "NOP", "IMP", 3), new Instruction("ADC", "ADC", "ZP0", 3), new Instruction("ROR", "ROR", "ZP0", 5), new Instruction("???", "XXX", "IMP", 5), new Instruction("PLA", "PLA", "IMP", 4), new Instruction("ADC", "ADC", "IMM", 2), new Instruction("ROR", "ROR", "IMP", 2), new Instruction("???", "XXX", "IMP", 2), new Instruction("JMP", "JMP", "IND", 5), new Instruction("ADC", "ADC", "ABS", 4), new Instruction("ROR", "ROR", "ABS", 6), new Instruction("???", "XXX", "IMP", 6),
            new Instruction("BVS", "BVS", "REL", 2), new Instruction("ADC", "ADC", "IZY", 5), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("???", "NOP", "IMP", 4), new Instruction("ADC", "ADC", "ZPX", 4), new Instruction("ROR", "ROR", "ZPX", 6), new Instruction("???", "XXX", "IMP", 6), new Instruction("SEI", "SEI", "IMP", 2), new Instruction("ADC", "ADC", "ABY", 4), new Instruction("???", "NOP", "IMP", 2), new Instruction("???", "XXX", "IMP", 7), new Instruction("???", "NOP", "IMP", 4), new Instruction("ADC", "ADC", "ABX", 4), new Instruction("ROR", "ROR", "ABX", 7), new Instruction("???", "XXX", "IMP", 7),
            new Instruction("???", "NOP", "IMP", 2), new Instruction("STA", "STA", "IZX", 6), new Instruction("???", "NOP", "IMP", 2), new Instruction("???", "XXX", "IMP", 6), new Instruction("STY", "STY", "ZP0", 3), new Instruction("STA", "STA", "ZP0", 3), new Instruction("STX", "STX", "ZP0", 3), new Instruction("???", "XXX", "IMP", 3), new Instruction("DEY", "DEY", "IMP", 2), new Instruction("???", "NOP", "IMP", 2), new Instruction("TXA", "TXA", "IMP", 2), new Instruction("???", "XXX", "IMP", 2), new Instruction("STY", "STY", "ABS", 4), new Instruction("STA", "STA", "ABS", 4), new Instruction("STX", "STX", "ABS", 4), new Instruction("???", "XXX", "IMP", 4),
            new Instruction("BCC", "BCC", "REL", 2), new Instruction("STA", "STA", "IZY", 6), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 6), new Instruction("STY", "STY", "ZPX", 4), new Instruction("STA", "STA", "ZPX", 4), new Instruction("STX", "STX", "ZPY", 4), new Instruction("???", "XXX", "IMP", 4), new Instruction("TYA", "TYA", "IMP", 2), new Instruction("STA", "STA", "ABY", 5), new Instruction("TXS", "TXS", "IMP", 2), new Instruction("???", "XXX", "IMP", 5), new Instruction("???", "NOP", "IMP", 5), new Instruction("STA", "STA", "ABX", 5), new Instruction("???", "XXX", "IMP", 5), new Instruction("???", "XXX", "IMP", 5),
            new Instruction("LDY", "LDY", "IMM", 2), new Instruction("LDA", "LDA", "IZX", 6), new Instruction("LDX", "LDX", "IMM", 2), new Instruction("???", "XXX", "IMP", 6), new Instruction("LDY", "LDY", "ZP0", 3), new Instruction("LDA", "LDA", "ZP0", 3), new Instruction("LDX", "LDX", "ZP0", 3), new Instruction("???", "XXX", "IMP", 3), new Instruction("TAY", "TAY", "IMP", 2), new Instruction("LDA", "LDA", "IMM", 2), new Instruction("TAX", "TAX", "IMP", 2), new Instruction("???", "XXX", "IMP", 2), new Instruction("LDY", "LDY", "ABS", 4), new Instruction("LDA", "LDA", "ABS", 4), new Instruction("LDX", "LDX", "ABS", 4), new Instruction("???", "XXX", "IMP", 4),
            new Instruction("BCS", "BCS", "REL", 2), new Instruction("LDA", "LDA", "IZY", 5), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 5), new Instruction("LDY", "LDY", "ZPX", 4), new Instruction("LDA", "LDA", "ZPX", 4), new Instruction("LDX", "LDX", "ZPY", 4), new Instruction("???", "XXX", "IMP", 4), new Instruction("CLV", "CLV", "IMP", 2), new Instruction("LDA", "LDA", "ABY", 4), new Instruction("TSX", "TSX", "IMP", 2), new Instruction("???", "XXX", "IMP", 4), new Instruction("LDY", "LDY", "ABX", 4), new Instruction("LDA", "LDA", "ABX", 4), new Instruction("LDX", "LDX", "ABY", 4), new Instruction("???", "XXX", "IMP", 4),
            new Instruction("CPY", "CPY", "IMM", 2), new Instruction("CMP", "CMP", "IZX", 6), new Instruction("???", "NOP", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("CPY", "CPY", "ZP0", 3), new Instruction("CMP", "CMP", "ZP0", 3), new Instruction("DEC", "DEC", "ZP0", 5), new Instruction("???", "XXX", "IMP", 5), new Instruction("INY", "INY", "IMP", 2), new Instruction("CMP", "CMP", "IMM", 2), new Instruction("DEX", "DEX", "IMP", 2), new Instruction("???", "XXX", "IMP", 2), new Instruction("CPY", "CPY", "ABS", 4), new Instruction("CMP", "CMP", "ABS", 4), new Instruction("DEC", "DEC", "ABS", 6), new Instruction("???", "XXX", "IMP", 6),
            new Instruction("BNE", "BNE", "REL", 2), new Instruction("CMP", "CMP", "IZY", 5), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("???", "NOP", "IMP", 4), new Instruction("CMP", "CMP", "ZPX", 4), new Instruction("DEC", "DEC", "ZPX", 6), new Instruction("???", "XXX", "IMP", 6), new Instruction("CLD", "CLD", "IMP", 2), new Instruction("CMP", "CMP", "ABY", 4), new Instruction("NOP", "NOP", "IMP", 2), new Instruction("???", "XXX", "IMP", 7), new Instruction("???", "NOP", "IMP", 4), new Instruction("CMP", "CMP", "ABX", 4), new Instruction("DEC", "DEC", "ABX", 7), new Instruction("???", "XXX", "IMP", 7),
            new Instruction("CPX", "CPX", "IMM", 2), new Instruction("SBC", "SBC", "IZX", 6), new Instruction("???", "NOP", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("CPX", "CPX", "ZP0", 3), new Instruction("SBC", "SBC", "ZP0", 3), new Instruction("INC", "INC", "ZP0", 5), new Instruction("???", "XXX", "IMP", 5), new Instruction("INX", "INX", "IMP", 2), new Instruction("SBC", "SBC", "IMM", 2), new Instruction("NOP", "NOP", "IMP", 2), new Instruction("???", "SBC", "IMP", 2), new Instruction("CPX", "CPX", "ABS", 4), new Instruction("SBC", "SBC", "ABS", 4), new Instruction("INC", "INC", "ABS", 6), new Instruction("???", "XXX", "IMP", 6),
            new Instruction("BEQ", "BEQ", "REL", 2), new Instruction("SBC", "SBC", "IZY", 5), new Instruction("???", "XXX", "IMP", 2), new Instruction("???", "XXX", "IMP", 8), new Instruction("???", "NOP", "IMP", 4), new Instruction("SBC", "SBC", "ZPX", 4), new Instruction("INC", "INC", "ZPX", 6), new Instruction("???", "XXX", "IMP", 6), new Instruction("SED", "SED", "IMP", 2), new Instruction("SBC", "SBC", "ABY", 4), new Instruction("NOP", "NOP", "IMP", 2), new Instruction("???", "XXX", "IMP", 7), new Instruction("???", "NOP", "IMP", 4), new Instruction("SBC", "SBC", "ABX", 4), new Instruction("INC", "INC", "ABX", 7), new Instruction("???", "XXX", "IMP", 7),
        ];
    }

    public fetch(): number {
        if (!(this.lookup[this.opcode].addrmode === "IMP"))
            this.fetched = this.read(this.addr_abs)
        return this.fetched;
    }

    public clock(): void {
        if (this.cycles === 0) {
            this.opcode = this.read(this.pc)
            this.pc += 1

            this.cycles = this.lookup[this.opcode].cycles;

            const additional_cycle1: number = this[this.lookup[this.opcode].addrmode]();
            const additional_cycle2: number = this[this.lookup[this.opcode].operate]();

            this.cycles += (additional_cycle1 & additional_cycle2)
        }
        this.cycles -= 1;
    }

    public reset(): void {
        this.addr_abs = 0xFFFC;
        const lo = this.read(this.addr_abs)
        const hi = this.read(this.addr_abs + 1)

        this.pc = (hi << 8) | lo
        this.a = 0
        this.x = 0
        this.y = 0
        this.stkp = 0xFD
        this.status = 0x00 | FLAGS6502.U
        this.addr_rel = 0x0000
        this.addr_abs = 0x0000
        this.fetched = 0x00
        this.cycles = 8
    }

    public irq(): void {
        if (this.getFlag(FLAGS6502.I) === 0) {
            this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF)
            this.stkp++
            this.write(0x0100 + this.stkp, this.pc & 0x00FF)
            this.stkp--

            this.setFlag(FLAGS6502.B, false)
            this.setFlag(FLAGS6502.U, true)
            this.setFlag(FLAGS6502.I, true)
            this.write(0x0100 + this.stkp, this.status)
            this.stkp--

            this.addr_abs = 0xFFFE
            const lo = this.read(this.addr_abs)
            const hi = this.read(this.addr_abs + 1)
            this.pc = (hi << 8) | lo
            this.cycles = 7
        }
    }

    public nmi(): void {
        this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF)
        this.stkp--
        this.write(0x0100 + this.stkp, this.pc & 0x00FF)
        this.stkp--

        this.setFlag(FLAGS6502.B, false)
        this.setFlag(FLAGS6502.U, true)
        this.setFlag(FLAGS6502.I, true)
        this.write(0x0100 + this.stkp, this.status)
        this.stkp--

        this.addr_abs = 0xFFFA
        const lo = this.read(this.addr_abs)
        const hi = this.read(this.addr_abs + 1)

        this.pc = (hi << 8) | lo
        this.cycles = 8
    }

    public connectBus(b: Bus) {
        this.bus = b;
    }

    public read(a: number): number {
        if (this.bus)
            return this.bus.read(a, false)
        return (0x00)
    }

    public write(a: number, d: number): void {
        if (this.bus)
            this.bus.write(a, d)
    }

    public setFlag(f: FLAGS6502, v: boolean): void {
        if (v) {
            this.status |= f;
        } else {
            this.status &= ~f;
        }
    }

    public getFlag(f: FLAGS6502): number {
        return ((this.status & f) > 0) ? 1 : 0
    }

    ADC() {

    }

    public AND(): number {
        this.fetch();
        this.a = this.a & this.fetched;
        this.setFlag(FLAGS6502.Z, this.a === 0x00)
        this.setFlag(FLAGS6502.N, (this.a & 0x80) > 0)
        return 1;
    }

    public ASL(): number {
        this.fetch()
        const temp = this.fetched << 1
        this.setFlag(FLAGS6502.C, (temp & 0xFF00) > 0)
        this.setFlag(FLAGS6502.Z, (temp & 0x00FF) === 0x00)
        this.setFlag(FLAGS6502.N, (temp & 0x80) > 0)

        if (this.lookup[this.opcode].addrmode === "IMP") {
            this.a = temp & 0x00FF
        } else {
            this.write(this.addr_abs, temp & 0x00FF)
        }
        return 0
    }

    public BCC(): number {
        if (this.getFlag(FLAGS6502.C) === 0) {
            this.cycles++;
            this.addr_abs = this.pc + this.addr_rel;
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00))
                this.cycles++
            this.pc = this.addr_abs;
        }
        return 0;
    }

    public BCS(): number {
        if (this.getFlag(FLAGS6502.C) === 1) {
            this.cycles++;
            this.addr_abs = this.pc + this.addr_rel;
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00))
                this.cycles++
            this.pc = this.addr_abs;
        }
        return 0;
    }

    public BEQ(): number {
        if (this.getFlag(FLAGS6502.Z) === 1) {
            this.cycles++;
            this.addr_abs = this.pc + this.addr_rel;
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00))
                this.cycles++
            this.pc = this.addr_abs;
        }
        return 0;
    }

    public BIT(): number {
        this.fetch()
        const temp = this.a & this.fetched
        this.setFlag(FLAGS6502.Z, (temp & 0x00FF) === 0x00)
    }

    BMI() {
        if (this.getFlag(FLAGS6502.N) === 1) {
            this.cycles++;
            this.addr_abs = this.pc + this.addr_rel;
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00))
                this.cycles++
            this.pc = this.addr_abs;
        }
        return 0;
    }

    BNE() {
        if (this.getFlag(FLAGS6502.Z) === 0) {
            this.cycles++;
            this.addr_abs = this.pc + this.addr_rel;
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00))
                this.cycles++
            this.pc = this.addr_abs;
        }
        return 0;
    }

    BPL() {
        if (this.getFlag(FLAGS6502.N) === 0) {
            this.cycles++;
            this.addr_abs = this.pc + this.addr_rel;
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00))
                this.cycles++
            this.pc = this.addr_abs;
        }
        return 0;
    }

    BRK() {
    }

    BVC() {
        if (this.getFlag(FLAGS6502.V) === 0) {
            this.cycles++;
            this.addr_abs = this.pc + this.addr_rel;
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00))
                this.cycles++
            this.pc = this.addr_abs;
        }
        return 0;
    }

    BVS() {
    }

    CLC() {
    }

    CLD() {
    }

    CLI() {
    }

    CLV() {
    }

    CMP() {
    }

    CPX() {
    }

    CPY() {
    }

    DEC() {
    }

    DEX() {
    }

    DEY() {
    }

    EOR() {
    }

    INC() {
    }

    INX() {
    }

    INY() {
    }

    JMP() {
    }

    JSR() {
    }

    LDA() {
    }

    LDX() {
    }

    LDY() {
    }

    LSR() {
    }

    NOP() {
    }

    ORA() {
    }

    PHA() {
    }

    PHP() {
    }

    PLA() {
    }

    PLP() {
    }

    ROL() {
    }

    ROR() {
    }

    RTI() {
    }

    RTS() {
    }

    SBC() {
    }

    SEC() {
    }

    SED() {
    }

    SEI() {
    }

    STA() {
    }

    STX() {
    }

    STY() {
    }

    TAX() {
    }

    TAY() {
    }

    TSX() {
    }

    TXA() {
    }

    TXS() {
    }

    TYA() {
    }

    // I capture all "unofficial" opcodes with this function. It is
    // functionally identical to a NOP
    XXX() {
    }
}

export default Cpu6502;
