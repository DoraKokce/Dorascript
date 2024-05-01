export enum TokenType {
    Number,
    String,
    Identifier,

    Equals,
    Semicolon,
    OpenParen,CloseParen,//()
    OpenBrace,CloseBrace,//{}
    OpenBracket,CloseBracket,//[]

    BinaryOperator,
    Comma,Colon,Dot,

    Const,
    Let,
    Function,

    EOF,
}

const KEYWORDS: Record<string, TokenType> = {
    let:TokenType.Let,
    const:TokenType.Const,
    function:TokenType.Function
}

const ESCAPED: Record<string, string> = {
    n: "\n",
    t: "\t",
    r: "\r",
};

export interface Token {
    value: string,
    type: TokenType,
    raw?: string;
}

function token(value = "", type:TokenType,raw:string = value): Token {
    return {
        value,
        type,
        raw,
    };
}

function isalpha(src:string) {
    return src.toUpperCase() != src.toLowerCase();
}

function isint(str:string) {
    const c = str.charCodeAt(0);
    const bounds = ['0'.charCodeAt(0),'9'.charCodeAt(0)]
    return (c >= bounds[0] && c <= bounds[1]);
}

function isskippable(str:string) {
    return str == ' ' || str == '\t' || str == '\n' || str == '\r';
}

export function tokenize(sourceCode:string): Token[] {
    const tokens = new Array<Token>();
    const src = sourceCode.split("");

    while (src.length > 0) {
        if (src[0] == '(') {
            tokens.push(token(src.shift(),TokenType.OpenParen));
        } else if (src[0] == ')') {
            tokens.push(token(src.shift(),TokenType.CloseParen));
        } else if (src[0] == '{') {
            tokens.push(token(src.shift(),TokenType.OpenBrace));
        } else if (src[0] == '}') {
            tokens.push(token(src.shift(),TokenType.CloseBrace));
        } else if (src[0] == '[') {
            tokens.push(token(src.shift(),TokenType.OpenBracket));
        } else if (src[0] == ']') {
            tokens.push(token(src.shift(),TokenType.CloseBracket));
        } else if (src[0] == '+' || src[0] == '-' || src[0] == '*' || src[0] == '/' || src[0] == '%') {
            tokens.push(token(src.shift(),TokenType.BinaryOperator));
        } else if (src[0] == '=') {
            tokens.push(token(src.shift(),TokenType.Equals));
        } else if (src[0] == ';') {
            tokens.push(token(src.shift(),TokenType.Semicolon))
        } else if (src[0] == ':') {
            tokens.push(token(src.shift(),TokenType.Colon))
        } else if (src[0] == ',') {
            tokens.push(token(src.shift(),TokenType.Comma))
        } else if (src[0] == '.') {
            tokens.push(token(src.shift(),TokenType.Dot))
        } else {
            if (isint(src[0])) {
                let num = "";
                while (src.length > 0 && isint(src[0])) {
                    num += src.shift();
                }
                tokens.push(token(num,TokenType.Number));
            } else if (isalpha(src[0])) {
                let ident = "";
                while (src.length > 0 && isalpha(src[0])) {
                    ident += src.shift();
                }
                
                const reserved = KEYWORDS[ident];
                
                if (typeof reserved == "number") {
                    tokens.push(token(ident,reserved));
                } else {
                    tokens.push(token(ident,TokenType.Identifier));
                }
            } else if (isskippable(src[0])) {
                src.shift();
            } else if (src[0] == '"') {
                let str = "";
                let raw = "";
                src.shift();

                let escaped = false;
                while (src.length > 0) {
                    const key = src.shift();
                    raw += key;
                    if(key == "\\") {
                        escaped = !escaped;
                        if(escaped)continue;
                    } else if (key == '"') {
                        if(!escaped) {
                            break;
                        }
                        escaped = false;
                    } else if (escaped) {
                        escaped = false;
                        if(ESCAPED[key]) {
                            str += ESCAPED[key];
                            continue;
                        } else {
                            str += `\\`;
                        }
                    }
                    str += key;  
                }
                
                tokens.push(token(str, TokenType.String, raw.substring(0, raw.length - 1)));
            } else {
                console.log("Invalid character found in source:",src[0])
                Deno.exit(1);
            }
        }
    }
    tokens.push(token("EOF",TokenType.EOF));
    return tokens;
}