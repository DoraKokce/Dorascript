import { Stmt } from "../frontend/ast.ts";
import Environment from "./environment.ts";

export type ValueType = 
    | "null" 
    | "number"
    | "boolean"
    | "object"
    | "native-fn"
    | "function";

export interface RuntimeVal {
    type: ValueType;
}

export interface NullVal extends RuntimeVal {
    type: "null";
    value: null;
}

export interface NumberVal extends RuntimeVal {
    type: "number";
    value: number;
}

export interface BooleanVal extends RuntimeVal {
    type: "boolean";
    value: boolean;
}

export interface ObjectVal extends RuntimeVal{
    type: "object";
    properties: Map<string,RuntimeVal>;
}

export type FunctionCall = (args: RuntimeVal[], env: Environment) => RuntimeVal;

export interface NativeFnValue extends RuntimeVal {
    type: "native-fn";
    call: FunctionCall;
}

export interface FunctionValue extends RuntimeVal {
    type: "function";
    name:string;
    paremeters:string[];
    declarationEnv:Environment;
    body: Stmt[];
}

export function make_native_func(call: FunctionCall) {
    return {
        type: "native-fn",
        call
    } as NativeFnValue;
}

export function make_null() {
    return {
        type: "null",
        value: null,
    } as NullVal;
}

export function make_bool(bool:boolean = true) {
    return {
        type: "boolean",
        value: bool,
    } as BooleanVal;
}

export function make_number(value = 0) {
    return {
        type: "number",
        value: value,
    } as NumberVal;
}