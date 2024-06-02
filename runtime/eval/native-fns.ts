import { RuntimeVal, StringVal, NumberVal, BooleanVal, NullVal, ObjectVal, FunctionValue, ArrayVal, make_null, make_bool, make_number, make_str, make_array } from '../values.ts'

export function printValues(args: Array<RuntimeVal>) {
    for (let i = 0; i < args.length; i++) {
        const arg = args[i];

        console.log(runtimeToJS(arg));
    }
}

export function runtimeToJS(arg: RuntimeVal) {
    switch (arg.type) {
        case "string":
            return (arg as StringVal).value;
        case "number":
            return (arg as NumberVal).value;
        case "boolean":
            return (arg as BooleanVal).value;
        case "null":
            return (arg as NullVal).value;
        case "object": {
            const obj: { [key: string]: unknown } = {};
            const aObj = arg as ObjectVal;
            aObj.properties.forEach((value, key) => obj[key] = runtimeToJS(value));
            return obj;
        }
        case "array": {
            const arr: unknown[] = [];
            const aArr = arg as ArrayVal;
            aArr.values.forEach(value => arr.push(runtimeToJS(value)));
            return arr;
        }
        case 'function': {
            const fn = arg as FunctionValue;
            return fn.name == "<anonymous>" ? `[Function (anonymous)]` : `[Function: ${fn.name}]`;
        }
        case "native-fn": {
            return `[Native Function]`;
        }
        default:
            return arg;
    }
}

export function jsToRuntime(val: unknown): RuntimeVal {
    if(val == null) return make_null();

    switch(typeof val) {
        case "boolean":
            return make_bool(val);
        case "bigint":
        case "number":
            return make_number(val as number);
        case "string":
            return make_str(val);
        case "object": {
            if(Array.isArray(val)) {
                const arr: RuntimeVal[] = [];
                val.forEach(value => {
                    arr.push(jsToRuntime(value));
                });
                return make_array(arr);
            }
            const prop = new Map<string, RuntimeVal>();
            Object.keys(val as Record<string, unknown>).forEach(key => {
                prop.set(key, jsToRuntime((val as Record<string, unknown>)[key]));
            });
            return Object(prop);
        }

        default:
            return make_null();
    }
}