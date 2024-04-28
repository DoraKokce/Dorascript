import Parser from "./frontend/parser.ts";
import Environment, { createGlobalEnv } from "./runtime/environment.ts";
import { evaluate } from "./runtime/interpreter.ts";


async function repl() {
    console.log("Dorascipt Demo")
    const parser = new Parser();
    const env = createGlobalEnv();

    while (true) {
        const input = prompt("> ");

        if (!input || input.includes("exit")) {
            Deno.exit(0);
        }
        const program = parser.produceAST(input);

        const result = evaluate(program,env);
        //console.log(result);
    }
}

repl()