import Parser from "./frontend/parser.ts";
import { createGlobalEnv } from "./runtime/environment.ts";
import { evaluate } from "./runtime/interpreter.ts";

async function run(filename:string) {
    const parser = new Parser();
    const env = createGlobalEnv();
    const input = await Deno.readTextFile(filename);
    const program = parser.produceAST(input);
    evaluate(program,env);
}

async function repl() {
    console.log("Dorascipt Demo")
    const parser = new Parser();
    const env = createGlobalEnv();

    while (true) {
        const input = prompt("> ");

        if (!input) {
            Deno.exit(0);
        }
        const program = parser.produceAST(input);

        evaluate(program,env);
    }
}

if (Deno.args[0]) {
    run(Deno.args[0])
} else {
    repl();
}