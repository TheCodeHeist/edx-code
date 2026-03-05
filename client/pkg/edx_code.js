/* @ts-self-types="./edx_code.d.ts" */

import * as wasm from "./edx_code_bg.wasm";
import { __wbg_set_wasm } from "./edx_code_bg.js";
__wbg_set_wasm(wasm);
wasm.__wbindgen_start();
export {
    to_python
} from "./edx_code_bg.js";
