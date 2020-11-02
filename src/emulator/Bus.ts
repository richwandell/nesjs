import Cpu6502 from "./Cpu6502";

class Bus {

    public cpu = new Cpu6502();

    public ram = new Uint8Array(64 * 1024);

    constructor() {
        this.cpu.connectBus(this)
    }

    public write(addr: number, data: number): void {
        if (addr >= 0x0000 && addr <= 0xFFFF)
            this.ram[addr] = data;
    }

    public read(addr: number, bReadOnly: boolean): number {
        if (addr >= 0x0000 && addr <= 0xFFFF)
            return this.ram[addr]
        return 0x00
    }
}

export default Bus;
