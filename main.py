#####################################################################################################################
# THIS SCRIPT WAS USED AS A PROTOTYPE AND FOR TESTING PURPOSES DURING DEVELOPMENT. IT IS NOT THE FINAL INTERPRETER. #
#####################################################################################################################

import argparse, json
from enum import Enum

parser = argparse.ArgumentParser(
    prog="Edx Interpreter",
    description="Interpreter for Pearson Edexcel IGCSE Computer Science Pseudocode",
    epilog="Developed by Md Riyasat Hossain <@TheCodeHeist>",
)
parser.add_argument("file", help="Path to the .edx file to interpret")
args = parser.parse_args()

RESERVED_KEYWORDS = [
    "SET",
    "TO",
    "IF",
    "THEN",
    "ELSE",
    "END",
    "WHILE",
    "DO",
    "REPEAT",
    "UNTIL",
    "TIMES",
    "FOR",
    "FOREACH",
    "FROM",
    "SEND",
    "RECEIVE",
    "READ",
    "WRITE",
    "PROCEDURE",
    "BEGIN",
    "RETURN",
]

RESERVED_DEVICES = [
    "KEYBOARD",
    "DISPLAY",
]

TYPES = [
    "INTEGER",
    "REAL",
    "CHARACTER",
    "STRING",
    "BOOLEAN",
    "ARRAY",
]

ARITHMETIC_OPERATORS = [
    "+",  #  Addition
    "-",  #  Subtraction
    "*",  #  Multiplication
    "/",  #  Division
    "^",  #  Exponentiation
    "&",  #  String Concatenation
    "MOD",  #  Modulus
    "DIV",  #  Integer Division
]

RELATIONAL_OPERATORS = [
    "=",  #  Equal to
    "<>",  #  Not equal to (alternative)
    ">",  #  Greater than
    ">=",  #  Greater than or equal to
    "<",  #  Less than
    "<=",  #  Less than or equal to
]

LOGICAL_OPERATORS = [
    "AND",
    "OR",
    "NOT",
]

OPERATION_BINDING_POWER = {
    "+": (1.0, 1.1),
    "-": (1.0, 1.1),
    "*": (2.0, 2.1),
    "/": (2.0, 2.1),
}


class TokenizerState(Enum):
    READY = 1
    WORD = 2
    NUMERAL = 3
    LITERAL = 4


class TokenType(Enum):
    IDENTIFIER = 1
    KEYWORD = 2
    DEVICE_REFERENCE = 3
    NUMERAL = 4
    LITERAL = 5
    OPERATOR = 6
    LEFT_PAREN = 7
    RIGHT_PAREN = 8
    LEFT_CURLY_BRACE = 9
    RIGHT_CURLY_BRACE = 10
    COMMA = 11
    SYMBOL = 12
    EOF = 13


class ObjectType(Enum):
    BINARY_EXPRESSION = 1
    KEYWORD = 2
    TYPE = 3
    REFERENCE = 4
    INTEGER = 5
    REAL = 6
    CHARACTER = 7
    STRING = 8
    BOOLEAN = 9
    ARRAY = 10
    ARITHMETIC_OPERATOR = 11
    RELATIONAL_OPERATOR = 12
    LOGICAL_OPERATOR = 13
    LEFT_PAREN = 14
    RIGHT_PAREN = 15


class ExpressionType(Enum):
    OPERATION = 1
    ATOM = 2


class Token:
    def __init__(self, type_: TokenType, value):
        self.type = type_
        self.value = value

    def __repr__(self):
        return f'Token -> ({self.type}) "{self.value}"'


class Object:
    def __init__(self, type_: ObjectType, value):
        self.type = type_
        self.value = value

    def __repr__(self):
        return f"Object -> ({self.type}) {self.value}"


class Expression:
    def __init__(self, type_: ExpressionType, operator, operands: list):
        self.type = type_
        self.operator = operator
        self.operands = operands

    def __repr__(self):
        if self.type == ExpressionType.ATOM:
            return f"{self.operator}"
        else:
            return f"({self.operator} {' '.join(str(op) for op in self.operands)})"

    # def rpn(self, level=0):


tokens: list[Token] = []


def next_token():
    global tokens

    if len(tokens) == 0:
        return Token(TokenType.EOF, None)
    else:
        return tokens.pop()


def peek_token():
    global tokens

    if len(tokens) == 0:
        return Token(TokenType.EOF, None)
    else:
        return tokens[-1]


def fetch_source(file_path):
    with open(file_path, "r") as file:
        return file.read()


def tokenizer(src):
    tokens = []

    pos = 0
    state = TokenizerState.READY
    current_lexeme = ""

    while pos < len(src):
        char = src[pos]

        if state == TokenizerState.READY:
            if char.isspace():
                pos += 1
                continue
            elif char == "#":
                while pos < len(src) and src[pos] != "\n":
                    pos += 1
            elif char in "(),[]{}":
                symbol_map = {
                    "(": TokenType.LEFT_PAREN,
                    ")": TokenType.RIGHT_PAREN,
                    "[": TokenType.LEFT_CURLY_BRACE,
                    "]": TokenType.RIGHT_CURLY_BRACE,
                    "{": TokenType.LEFT_CURLY_BRACE,
                    "}": TokenType.RIGHT_CURLY_BRACE,
                    ",": TokenType.COMMA,
                }
                tokens.append(Token(symbol_map[char], char))

            # Handle two-character symbols
            elif char in "<":
                next_char = src[pos + 1] if pos + 1 < len(src) else ""
                if next_char == "=":
                    tokens.append(Token(TokenType.OPERATOR, "<="))
                    pos += 2
                elif next_char == ">":
                    tokens.append(Token(TokenType.OPERATOR, "<>"))
                    pos += 2
                else:
                    tokens.append(Token(TokenType.OPERATOR, "<"))
                    pos += 1

            elif char in ">":
                next_char = src[pos + 1] if pos + 1 < len(src) else ""
                if next_char == "=":
                    tokens.append(Token(TokenType.OPERATOR, ">="))
                    pos += 2
                else:
                    tokens.append(Token(TokenType.OPERATOR, ">"))
                    pos += 1

            elif char in "=":
                tokens.append(Token(TokenType.OPERATOR, "="))
                pos += 1

            elif char in ARITHMETIC_OPERATORS:
                tokens.append(Token(TokenType.OPERATOR, char))
                pos += 1

            # Handle string literals

            elif char == '"':
                state = TokenizerState.LITERAL
                current_lexeme = ""
                pos += 1

                while pos < len(src) and src[pos] != '"':
                    current_lexeme += src[pos]
                    pos += 1

                tokens.append(Token(TokenType.LITERAL, current_lexeme))

                current_lexeme = ""
                state = TokenizerState.READY

            elif char == "'":
                state = TokenizerState.LITERAL
                current_lexeme = ""
                pos += 1

                while pos < len(src) and src[pos] != "'":
                    current_lexeme += src[pos]
                    pos += 1

                tokens.append(Token(TokenType.LITERAL, current_lexeme))

                current_lexeme = ""
                state = TokenizerState.READY
            elif char.isalpha():
                state = TokenizerState.WORD
                current_lexeme += char
            elif char.isdigit():
                state = TokenizerState.NUMERAL
                current_lexeme += char
            else:
                tokens.append(Token(TokenType.SYMBOL, char))

        elif state == TokenizerState.WORD:
            if char.isalnum() or char == "_":
                current_lexeme += char
            else:
                if current_lexeme.upper() in RESERVED_KEYWORDS:
                    tokens.append(Token(TokenType.KEYWORD, current_lexeme.upper()))
                elif current_lexeme.upper() in RESERVED_DEVICES:
                    tokens.append(
                        Token(TokenType.DEVICE_REFERENCE, current_lexeme.upper())
                    )
                elif (
                    current_lexeme.upper()
                    in ARITHMETIC_OPERATORS + RELATIONAL_OPERATORS + LOGICAL_OPERATORS
                ):
                    tokens.append(Token(TokenType.OPERATOR, current_lexeme.upper()))
                else:
                    tokens.append(Token(TokenType.IDENTIFIER, current_lexeme))

                current_lexeme = ""
                state = TokenizerState.READY
                continue  # Re-evaluate this character

        elif state == TokenizerState.NUMERAL:
            if char.isdigit():
                current_lexeme += char

            elif char == ".":
                if "." in current_lexeme:
                    raise ValueError("Invalid number format")

                current_lexeme += char

            elif char.isalpha():
                raise ValueError("Invalid character in suspected numeral")

            else:
                tokens.append(Token(TokenType.NUMERAL, current_lexeme))
                current_lexeme = ""
                state = TokenizerState.READY
                continue  # Re-evaluate this character

        pos += 1

    tokens.append(Token(TokenType.EOF, None))

    return tokens


def parse_expression(min_bp=0):
    lhs = None

    curr_token = next_token()

    if (
        curr_token.type == TokenType.NUMERAL
        or curr_token.type == TokenType.IDENTIFIER
        or curr_token.type == TokenType.LITERAL
    ):
        lhs = Expression(
            ExpressionType.ATOM,
            curr_token.value,
            [],
        )
    else:
        raise ValueError("Unexpected token:", curr_token)

    # print("Initial LHS:", lhs)

    while True:
        op = None

        # print("Tokens at loop start:", tokens)
        incoming_token = peek_token()

        if incoming_token.type == TokenType.EOF:
            break
        elif incoming_token.type == TokenType.OPERATOR:
            op = incoming_token
        else:
            raise ValueError("Unexpected token in expression:", incoming_token)

        next_token()

        lbp, rbp = OPERATION_BINDING_POWER.get(op.value, (0, 0))
        if lbp < min_bp:
            break

        rhs = parse_expression(rbp)

        lhs = Expression(
            ExpressionType.OPERATION,
            op.value,
            [lhs, rhs],
        )

    return lhs


def parser(tokens: list[Token]):
    objects = []

    current_token = 0
    halt_flags = {
        "IN_ARRAY": False,
        "IN_PROCEDURE": False,
        # "IN_EXPRESSION": False,
    }
    new_array = []
    expression_stack = []

    while current_token < len(tokens):
        token = tokens[current_token]

        if token.type == "IDENTIFIER":
            if token.value in RESERVED_KEYWORDS:
                if len(expression_stack) > 0:
                    objects.append(Object("EXPRESSION_SEQ", expression_stack))
                    expression_stack = []

                objects.append(Object("KEYWORD", token.value))

            elif token.value in TYPES:
                expression_stack.append(Object("TYPE", token.value))

            elif token.value.upper() in ["TRUE", "FALSE"]:
                expression_stack.append(
                    Object("BOOLEAN", token.value.upper() == "TRUE")
                )
            elif token.value in ARITHMETIC_OPERATORS:
                expression_stack.append(Object("ARITHMETIC_OPERATOR", token.value))
            elif token.value in LOGICAL_OPERATORS:
                expression_stack.append(Object("LOGICAL_OPERATOR", token.value))
            else:
                expression_stack.append(Object("REFERENCE", token.value))

        elif token.type == "NUMERAL":
            if "." in token.value:
                if halt_flags["IN_ARRAY"]:
                    # print("FLOAT DETECTED IN ARRAY")
                    new_array.append(float(token.value))

                    continue

                expression_stack.append(Object("REAL", float(token.value)))

            else:
                if halt_flags["IN_ARRAY"]:
                    # print("INTEGER DETECTED IN ARRAY")
                    new_array.append(int(token.value))

                    current_token += 1
                    continue

                expression_stack.append(Object("INTEGER", int(token.value)))

        elif token.type == "LITERAL":
            if halt_flags["IN_ARRAY"]:
                # print("STRING DETECTED IN ARRAY")
                new_array.append(token.value)

                current_token += 1
                continue

            if len(token.value) == 1:
                expression_stack.append(Object("CHARACTER", token.value))
            else:
                expression_stack.append(Object("STRING", token.value))

        elif token.type == "SYMBOL":
            # print("SYMBOL DETECTED:", token.value)
            if token.value == "[":
                if halt_flags["IN_ARRAY"]:
                    raise ValueError("Nested arrays are not supported")
                else:
                    halt_flags["IN_ARRAY"] = True
                    new_array = []

            elif token.value == ",":
                # print("COMMA DELIMITED ARRAY")
                pass

            elif token.value == "]":
                if not halt_flags["IN_ARRAY"]:
                    raise ValueError("Unexpected closing bracket ']'")

                # print("ARRAY CLOSED:", new_array)
                halt_flags["IN_ARRAY"] = False
                expression_stack.append(Object("ARRAY", new_array))
                new_array = []

            elif token.value == "+":
                expression_stack.append(Object("ARITHMETIC_OPERATOR", "ADD"))

            elif token.value == "-":
                expression_stack.append(Object("ARITHMETIC_OPERATOR", "SUBTRACT"))

            elif token.value == "*":
                expression_stack.append(Object("ARITHMETIC_OPERATOR", "MULTIPLY"))

            elif token.value == "/":
                expression_stack.append(Object("ARITHMETIC_OPERATOR", "DIVIDE"))

            elif token.value == "^":
                expression_stack.append(Object("ARITHMETIC_OPERATOR", "EXPONENTIATION"))

            elif token.value == "&":
                expression_stack.append(Object("ARITHMETIC_OPERATOR", "STRCAT"))

            elif token.value == "=":
                expression_stack.append(Object("RELATIONAL_OPERATOR", "EQUALS"))

            elif token.value == "<":
                if (
                    current_token + 1 < len(tokens)
                    and tokens[current_token + 1].type == "SYMBOL"
                ):
                    if tokens[current_token + 1].value == ">":
                        expression_stack.append(
                            Object("RELATIONAL_OPERATOR", "NOT_EQUAL")
                        )
                    elif tokens[current_token + 1].value == "=":
                        expression_stack.append(
                            Object("RELATIONAL_OPERATOR", "LESS_THAN_EQUAL")
                        )

                    current_token += 1  # Skip the token
                else:
                    expression_stack.append(Object("RELATIONAL_OPERATOR", "LESS_THAN"))

            elif token.value == ">":
                if (
                    current_token + 1 < len(tokens)
                    and tokens[current_token + 1].type == "SYMBOL"
                    and tokens[current_token + 1].value == "="
                ):
                    expression_stack.append(
                        Object("RELATIONAL_OPERATOR", "GREATER_THAN_EQUAL")
                    )
                    current_token += 1  # Skip the token
                else:
                    expression_stack.append(
                        Object("RELATIONAL_OPERATOR", "GREATER_THAN")
                    )

            elif token.value == "(":
                expression_stack.append(Object("LEFT_PAREN", "("))
            elif token.value == ")":
                expression_stack.append(Object("RIGHT_PAREN", ")"))

        elif token.type == "EOF":
            if len(expression_stack) > 0:
                objects.append(Object("EXPRESSION_SEQ", expression_stack))
                expression_stack = []

            break

        current_token += 1

    return objects


# def generate_bytecode(objects: list[Object]):
#     bytecode = []

#     current_object = 0

#     while current_object < len(objects):
#         obj = objects[current_object]

#         if obj.type == "KEYWORD" and obj.value == "SET":
#             # Expecting: SET <EXPR<REFERENCE>> TO <EXPR>
#             reference_obj = objects[current_object + 1]
#             to_obj = objects[current_object + 2]
#             value_obj = objects[current_object + 3]

#             if to_obj.type != "KEYWORD" or to_obj.value != "TO":
#                 raise ValueError(
#                     "Expected 'TO' keyword after reference in SET statement"
#                 )

#             if reference_obj.type != "EXPRESSION_SEQ":
#                 raise ValueError("Expected a reference after 'SET' keyword")

#             if (
#                 len(reference_obj.value) != 1
#                 or reference_obj.value[0].type != "REFERENCE"
#             ):
#                 raise ValueError("SET statement must assign to a single reference")

#             if value_obj.type != "EXPRESSION_SEQ":
#                 raise ValueError("Invalid value type in SET statement:", value_obj.type)

#             bytecode.append(
#                 {
#                     "operation": "VariableDeclaration",
#                     "variable": reference_obj.value[0].value,
#                     "type": value_obj.type,
#                     "value": value_obj.value,
#                 }
#             )

#             current_object += 4
#             continue

#         elif obj.type == "KEYWORD" and obj.value == "SEND":
#             # Expecting: SEND <EXPR> TO <EXPR<REFERENCE>>

#             value_obj = objects[current_object + 1]
#             to_obj = objects[current_object + 2]
#             reference_obj = objects[current_object + 3]

#             if to_obj.type != "KEYWORD" or to_obj.value != "TO":
#                 raise ValueError("Expected 'TO' keyword after value in SEND statement")

#             if reference_obj.type != "EXPRESSION_SEQ":
#                 raise ValueError(
#                     "Expected a reference after 'TO' keyword in SEND statement"
#                 )

#             if (
#                 len(reference_obj.value) != 1
#                 or reference_obj.value[0].type != "REFERENCE"
#             ):
#                 raise ValueError("SEND statement must send to a single reference")

#             if value_obj.type != "EXPRESSION_SEQ":
#                 raise ValueError(
#                     "Invalid value type in SEND statement:", value_obj.type
#                 )

#             bytecode.append(
#                 {
#                     "operation": "Output",
#                     "to": reference_obj.value[0].value,
#                     "value": value_obj.value,
#                 }
#             )

#             current_object += 4
#             continue

#         elif obj.type == "KEYWORD" and obj.value == "RECEIVE":
#             # Expecting: RECEIVE <EXPR<REFERENCE>> FROM <EXPR <LEFT_PAREN> <TYPE> <RIGHT_PAREN> <REFERENCE>>

#             reference_obj = objects[current_object + 1]
#             from_obj = objects[current_object + 2]
#             type_and_source_obj = objects[current_object + 3]

#             if from_obj.type != "KEYWORD" or from_obj.value != "FROM":
#                 raise ValueError(
#                     "Expected 'FROM' keyword after reference in RECEIVE statement"
#                 )

#             if reference_obj.type != "EXPRESSION_SEQ":
#                 raise ValueError("Expected a reference after 'RECEIVE' keyword")

#             if (
#                 len(reference_obj.value) != 1
#                 or reference_obj.value[0].type != "REFERENCE"
#             ):
#                 raise ValueError("RECEIVE statement must assign to a single reference")

#             if type_and_source_obj.type != "EXPRESSION_SEQ":
#                 raise ValueError(
#                     "Expected type and source declaration in RECEIVE statement"
#                 )

#             if (
#                 len(type_and_source_obj.value) > 4
#                 or type_and_source_obj.value[3].type != "REFERENCE"
#             ):
#                 raise ValueError("RECEIVE statement source must be a single reference")

#             bytecode.append(
#                 {
#                     "operation": "Input",
#                     "variable": reference_obj.value[0].value,
#                     "type": type_and_source_obj.value[1].value,
#                     "source": type_and_source_obj.value[3].value,
#                 }
#             )

#             current_object += 5
#             continue

#         elif obj.type == "KEYWORD" and obj.value == "IF":
#             # Expecting: IF <EXPR> THEN
#             condition_obj = objects[current_object + 1]
#             then_obj = objects[current_object + 2]

#             if then_obj.type != "KEYWORD" or then_obj.value != "THEN":
#                 raise ValueError(
#                     "Expected 'THEN' keyword after condition in IF statement"
#                 )

#             if condition_obj.type != "EXPRESSION_SEQ":
#                 raise ValueError(
#                     "Invalid condition type in IF statement:", condition_obj.type
#                 )

#             bytecode.append(
#                 {
#                     "operation": "IfStatement",
#                     "condition": condition_obj.value,
#                 }
#             )

#             current_object += 3
#             continue

#         elif obj.type == "KEYWORD" and obj.value == "ELSE":
#             bytecode.append(
#                 {
#                     "operation": "ElseStatement",
#                 }
#             )

#             current_object += 1
#             continue

#         elif obj.type == "KEYWORD" and obj.value == "END":
#             # Expecting: END <KEYWORD>
#             end_obj = objects[current_object + 1]

#             if end_obj.type != "KEYWORD":
#                 raise ValueError("Expected keyword after 'END'")

#             bytecode.append(
#                 {
#                     "operation": "EndStatement",
#                     "ends": end_obj.value,
#                 }
#             )

#             current_object += 2
#             continue

#         else:
#             # For unhandled objects, just append a NoOp for now
#             bytecode.append(
#                 {
#                     "operation": "NoOp",
#                     "details": f"Unhandled object type: {obj.type} with value: {obj.value}",
#                 }
#             )

#         current_object += 1

#     return bytecode


def main():
    global tokens

    # source_code = fetch_source(args.file)
    source_code = "a + b * 2 * c + a / 4 "

    tokens = tokenizer(source_code)
    tokens.reverse()
    # print("\n".join(str(token) for token in tokens))

    print(parse_expression())

    # objects = parser(tokens)
    # print(json.dumps(objects, default=lambda o: o.__dict__, indent=4))

    # bytecode = generate_bytecode(objects)
    # print(json.dumps(bytecode, default=lambda o: o.__dict__, indent=4))


if __name__ == "__main__":
    main()
