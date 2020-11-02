import React, {useEffect} from 'react';
import Bus from "./Bus";

function EmuCanvas() {

    let ref = React.createRef<HTMLCanvasElement>();

    useEffect(() => {
        if (!ref) return;
        let bus = new Bus();
        console.log(bus)
    }, [ref])

    return (
        <canvas ref={ref} />
    );
}

export default EmuCanvas;
