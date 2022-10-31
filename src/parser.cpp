/**
 *  CPSC 323 Compilers and Languages
 *
 *  Dalton Caron, Teaching Associate
 *  dcaron@fullerton.edu, +1 949-616-2699
 *  Department of Computer Science
 */
#include <parser.h>

Parser::Parser(const token_stream_t &tokens)
: tokens(tokens) {}

Parser::~Parser() {}

ParseTree<std::string> Parser::parse() {
    return this->parseProgram();
}

token_t Parser::getNextToken() {
    // Assumes that the end of the token stream always has
    // the end of file enumeration.
    if (this->current_token == this->tokens.size()) {
        return this->tokens[this->current_token - 1];
    }

    return this->tokens[this->current_token++];
}

token_t Parser::peekNextToken() const {
    if (this->current_token == this->tokens.size()) {
        return this->tokens[this->current_token - 1];
    }
    return this->tokens[this->current_token];
}

// This is the example that was provided in the document.
PTPtr<std::string> Parser::parseProgram() {
    PTPtr<std::string> programNode =
        std::make_shared<PTNode<std::string>>("program");
    programNode->addChild(this->parseBlock());
    this->tryMatchTerminal(this->getNextToken(), PERIOD, programNode);

    return programNode;
}

// START OF IMPLEMENTATION

/* Valid terms that can be identified by the parser:

"program", ".", "block", "statement", "end", "statement", "statement",
"end", "statement", "expression", "term", "factor", "1", "+", "term",
"factor", "x", ":=", "x", ";", "statement", "expression", "term",
"factor", "squ", "!", ";", "statement", "square", "call", "begin",
"do", "condition", "expression", "term", "factor", "10", "<=",
"expression", "term", "factor", "x", "while", ";", "statement",
"expression", "term", "factor", "1", ":=", "x", "begin", "procedure",
";", "block", "statement", "end", "statement", "expression", "term",
"factor", "x", "*", "factor", "x", ":=", "squ", "begin", ";", "square",
"procedure", "var_declaration", ";", "var_declaration_list", "squ", ",",
"x", "var" */

/* Token Identifers:

ERROR = 0,
IDENTIFIER = 1,
CONST_KEYWORD = 2,
NUMBER_LITERAL = 3,
COMMA = 4,
DEFINE_EQUALS = 5,
EQUALS = 6,
LEFT_PAREN = 7,
RIGHT_PAREN = 8,
ODD_OP = 9,
END_KEYWORD = 10,
BEGIN_KEYWORD = 11,
CALL_KEYWORD = 12,
SEMICOLON = 13,
PROCEDURE_KEYWORD = 14,
IF_KEYWORD = 15,
THEN_KEYWORD = 16,
WHILE_KEYWORD = 17,
DO_KEYWORD = 18,
ADD_OP = 19,
MUL_OP = 20,
COMPARE_OP = 21,
VAR_KEYWORD = 22,
WRITE_OP = 23,
READ_OP = 24,
PERIOD = 25,
END_OF_FILE = 26,
WHITESPACE = 27 */

/* The parser converts the tokens it receives into a tree data structure.
   When the parser identifies the "const" or "var" identifiers in the code,
   it will add those tokens to the block as its children.*/
PTPtr<std::string> Parser::parseBlock() {
    PTPtr<std::string> blockNode =
        std::make_shared<PTNode<std::string>>("block");
    PTPtr<std::string> constDeclNode =
        std::make_shared<PTNode<std::string>>("const_declaration");
    PTPtr<std::string> varDeclNode =
        std::make_shared<PTNode<std::string>>("var_declaration");
    PTPtr<std::string> procedureNode =
        std::make_shared<PTNode<std::string>>("procedure");

    token_t token;
    token = this->peekNextToken();

    /* Checks if "const" keyword is present in token.
       If the condition is true, then the program will go through
       the token stream stored until it reaches a semicolon (";").*/
    if (token.lexeme.compare("const") == 0) {
      constDeclNode->addChild(this->parseConstDeclarationList());
      this->tryMatchTerminal(token, SEMICOLON, constDeclNode);
    }

    /* Checks if "var" keyword is present in token.
       If the condition is true, then the program will go through the
       token stream stored until it reaches a semicolon (";").*/
    if (token.lexeme.compare("var") == 0) {
      varDeclNode->addChild(this->parseVarDeclarationList());
      this->tryMatchTerminal(token, {SEMICOLON, COMMA}, varDeclNode);
    }

    while (token.lexeme.compare("procedure") == 0) {
      procedureNode->addChild(this->parseStatement());
      this->tryMatchTerminal(token, SEMICOLON, procedureNode);
    }

    blockNode->addChild(this->parseStatement());
    this->tryMatchTerminal(token, SEMICOLON, blockNode);

    return blockNode;
}

PTPtr<std::string> Parser::parseConstDeclarations() {
    PTPtr<std::string> constDeclNode =
        std::make_shared<PTNode<std::string>>("const_declaration");
      constDeclNode->addChild(this->parseConstDeclarationList());
      this->tryMatchTerminal(this->getNextToken(), SEMICOLON, constDeclNode);

    return constDeclNode;
}

PTPtr<std::string> Parser::parseConstDeclarationList() {
    PTPtr<std::string> constDeclListNode =
        std::make_shared<PTNode<std::string>>("const_declaration_list");
    constDeclListNode->addChild(this->parseVarDeclarations());
    this->tryMatchTerminal(this->getNextToken(), NUMBER_LITERAL, constDeclListNode);

    return constDeclListNode;
}

PTPtr<std::string> Parser::parseVarDeclarations() {
    PTPtr<std::string> varDeclNode =
        std::make_shared<PTNode<std::string>>("var_declaration");
    varDeclNode->addChild(this->parseVarDeclarationList());
    this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, SEMICOLON}, varDeclNode);

    return varDeclNode;
}

PTPtr<std::string> Parser::parseVarDeclarationList() {
    PTPtr<std::string> varDeclNodeList =
        std::make_shared<PTNode<std::string>>("var_declaration_list");

    token_t token;
    token = this->peekNextToken();

    while (token.lexeme.compare("var") == 0) {

      if (token.lexeme.compare(";") == 0) {
        varDeclNodeList = std::make_shared<PTNode<std::string>>(";");
        varDeclNodeList->addChild(this->parseVarDeclarationList());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, varDeclNodeList);
      }

      if (token.type == IDENTIFIER) {
        varDeclNodeList = std::make_shared<PTNode<std::string>>(token.lexeme);
        varDeclNodeList->addChild(this->parseVarDeclarationList());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, varDeclNodeList);
      }
    }

    /*varDeclNodeList->addChild(this->parseProcedure());
    this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, varDeclNodeList);
*/
    return varDeclNodeList;
}

PTPtr<std::string> Parser::parseProcedure() {
    PTPtr<std::string> procedure =
        std::make_shared<PTNode<std::string>>("procedure");
      procedure->addChild(this->parseStatement());
      this->tryMatchTerminal(this->getNextToken(), SEMICOLON, procedure);

    return procedure;
}

PTPtr<std::string> Parser::parseStatement() {
    PTPtr<std::string> statementNode =
        std::make_shared<PTNode<std::string>>("statement");
    PTPtr<std::string> conditionNode =
        std::make_shared<PTNode<std::string>>("condition");
    PTPtr<std::string> expressionNode =
        std::make_shared<PTNode<std::string>>("expression");

    token_t token;
    token = this->peekNextToken();

    if (token.lexeme.compare(":=") == 0) {
      statementNode = std::make_shared<PTNode<std::string>>(":=");
      statementNode->addChild(this->parseCondition());
      this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);
    }
    else if (token.lexeme.compare("call") == 0) {
      expressionNode = std::make_shared<PTNode<std::string>>("call");
      expressionNode->addChild(this->parseTerm());
      this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
    }
    else if (token.lexeme.compare("?") == 0) {
      statementNode = std::make_shared<PTNode<std::string>>("?");
      statementNode->addChild(this->parseCondition());
      this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);
    }
    else if (token.lexeme.compare("!") == 0) {
      expressionNode = std::make_shared<PTNode<std::string>>("!");
      expressionNode->addChild(this->parseTerm());
      this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
    }
    else if (token.lexeme.compare("begin") == 0) {
      statementNode = std::make_shared<PTNode<std::string>>("begin");
      statementNode->addChild(this->parseCondition());

      while (token.lexeme.compare(";") == 0) {
        statementNode = std::make_shared<PTNode<std::string>>(";");
        statementNode->addChild(this->parseCondition());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);
      }

      this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);
    }
    else if (token.lexeme.compare("if") == 0) {
      conditionNode = std::make_shared<PTNode<std::string>>("if");
      conditionNode->addChild(this->parseExpression());
      this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, conditionNode);

      if (token.lexeme.compare("then") == 0) {
        statementNode = std::make_shared<PTNode<std::string>>("then");
        statementNode->addChild(this->parseCondition());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);
      }

    }

    else if (token.lexeme.compare("while") == 0) {
      statementNode = std::make_shared<PTNode<std::string>>("while");
      statementNode->addChild(this->parseCondition());
      this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);

      if (token.lexeme.compare("do") == 0) {
        statementNode = std::make_shared<PTNode<std::string>>("do");
        statementNode->addChild(this->parseCondition());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);
      }

    }

    return statementNode;
}

PTPtr<std::string> Parser::parseCondition() {
    PTPtr<std::string> conditionNode =
        std::make_shared<PTNode<std::string>>("condition");
    PTPtr<std::string> expressionNode =
        std::make_shared<PTNode<std::string>>("expression");

    token_t token;
    token = this->peekNextToken();

    if (token.type == ODD_OP) {
      expressionNode = std::make_shared<PTNode<std::string>>("odd");
      expressionNode->addChild(this->parseTerm());
      this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
    }
    else if (token.lexeme.compare("expression") == 0){
      if (token.lexeme.compare("=") == 0) {
        expressionNode = std::make_shared<PTNode<std::string>>(":=");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
      }
      else if (token.lexeme.compare("<") == 0) {
        expressionNode = std::make_shared<PTNode<std::string>>("<");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
      }
      else if (token.lexeme.compare("<=") == 0) {
        expressionNode = std::make_shared<PTNode<std::string>>("<=");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
      }
      else if (token.lexeme.compare(">") == 0) {
        expressionNode = std::make_shared<PTNode<std::string>>(">");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
      }
      else if (token.lexeme.compare(">=") == 0) {
        expressionNode = std::make_shared<PTNode<std::string>>(">=");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
      }
    }

    conditionNode->addChild(this->parseExpression());
    this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, conditionNode);

    return conditionNode;
}

PTPtr<std::string> Parser::parseExpression() {
    PTPtr<std::string> expressionNode =
        std::make_shared<PTNode<std::string>>("expression");
    PTPtr<std::string> termNode = std::make_shared<PTNode<std::string>>("term");

    token_t token;
    token = this->peekNextToken();

    if (token.lexeme.compare("+") == 0) {
      expressionNode = std::make_shared<PTNode<std::string>>("+");
      expressionNode->addChild(this->parseTerm());
      this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
    }
    else if (token.lexeme.compare("-") == 0) {
      expressionNode = std::make_shared<PTNode<std::string>>("-");
      expressionNode->addChild(this->parseTerm());
      this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
    }

    while (token.lexeme.compare("+") == 0) {
      if (token.lexeme.compare("+") == 0) {
        termNode = std::make_shared<PTNode<std::string>>("+");
        termNode->addChild(this->parseFactor());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);
      }
      if (token.lexeme.compare("-") == 0) {
        termNode = std::make_shared<PTNode<std::string>>("-");
        termNode->addChild(this->parseFactor());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);
      }
    }

    expressionNode->addChild(this->parseTerm());
    this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);

    return expressionNode;
}

PTPtr<std::string> Parser::parseTerm() {
    PTPtr<std::string> termNode = std::make_shared<PTNode<std::string>>("term");
    PTPtr<std::string> factorNode =
        std::make_shared<PTNode<std::string>>("factor");

    token_t token;
    token = this->peekNextToken();

    termNode->addChild(this->parseFactor());
    this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);

    while (token.lexeme.compare("factor") == 0) {
      if (token.lexeme.compare("*") == 0) {
        factorNode = std::make_shared<PTNode<std::string>>("*");
        termNode->addChild(this->parseFactor());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);
      }

      else if (token.lexeme.compare("/") == 0) {
        factorNode = std::make_shared<PTNode<std::string>>("/");
        termNode->addChild(this->parseFactor());
        this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);
      }
    }

    /*termNode->addChild(this->parseFactor());
    this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);
    */

    return termNode;
}

PTPtr<std::string> Parser::parseFactor() {
    PTPtr<std::string> factorNode =
        std::make_shared<PTNode<std::string>>("factor");
    this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, factorNode);

    return factorNode;
}

// END OF IMPLEMENTATION

void Parser::tryMatchTerminal(
    const token_t &actual,
    const token_class_t expected,
    PTPtr<std::string> node
) const {
    if (actual.type != expected) {
        raiseMismatchError(actual, expected);
    }
    node->addChild(actual.lexeme);
}

void Parser::tryMatchTerminal(
    const token_t &actual,
    const std::initializer_list<token_class_t> expected,
    PTPtr<std::string> node
) const {
    for (const token_class_t type : expected) {
        if (type == actual.type) {
            node->addChild(actual.lexeme);
            return;
        }
    }
    raiseMismatchError(actual, expected);
}

void Parser::raiseMismatchError(
        const token_t &actual,
        const token_class_t expected
) const {
    ERROR_LOG(
        "expected %s got %s "
        "at %s line %d column %d",
        tokenTypeToString(expected).c_str(),
        actual.lexeme.c_str(),
        actual.file.c_str(),
        actual.line,
        actual.column
    );

    exit(EXIT_FAILURE);
}

void Parser::raiseMismatchError(
        const token_t &actual,
        const std::initializer_list<token_class_t> expected
) const {
    ERROR_LOG("expected ");

    for (auto token : expected) {
        (void) ERROR_LOG(
            "%s ",
            tokenTypeToString(token).c_str()
        );
    }

    ERROR_LOG(
        "got %s at %s line %d column %d",
        actual.lexeme.c_str(),
        actual.file.c_str(),
        actual.line,
        actual.column
    );

    exit(EXIT_FAILURE);
}
