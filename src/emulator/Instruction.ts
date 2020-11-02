class Instruction {

    public name: string;
    public operate: string;
    public addrmode: string;
    public cycles: number;

    constructor(name: string, operate: string, addrmode: string, cycles: number) {
        this.name = name;
        this.operate = operate;
        this.addrmode = addrmode;
        this.cycles = cycles
    }
}

export default Instruction;
