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

PTPtr<std::string> Parser::parseBlock() {
    PTPtr<std::string> blockNode =
        std::make_shared<PTNode<std::string>>("block");
    blockNode->addChild(this->parseConstDeclarations());
    blockNode->addChild(this->parseVarDeclarations());
    blockNode->addChild(this->parseProcedure());
    blockNode->addChild(this->parseStatement());



    //this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, blockNode);

    return blockNode;
}

PTPtr<std::string> Parser::parseConstDeclarations() {
    PTPtr<std::string> constDeclNode =
        std::make_shared<PTNode<std::string>>("const_declaration");

      constDeclNode->addChild(this->parseConstDeclarationList());

      /*if (constDeclNode == CONST_KEYWORD) {
        // ACTIONS
      }*/

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

        /*

        if (varDeclNode == VAR_KEYWORD) {

          ACTIONS
        }

        */



        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, varDeclNode);
    return varDeclNode;
}

PTPtr<std::string> Parser::parseVarDeclarationList() {
    PTPtr<std::string> varDeclNodeList =
        std::make_shared<PTNode<std::string>>("var_declaration_list");
        varDeclNodeList->addChild(this->parseProcedure());
        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, varDeclNodeList);
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
        statementNode->addChild(this->parseCondition());
        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);
    return statementNode;
}

PTPtr<std::string> Parser::parseCondition() {
    PTPtr<std::string> conditionNode =
        std::make_shared<PTNode<std::string>>("condition");
     conditionNode->addChild(this->parseExpression());
     this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, conditionNode);
    return conditionNode;
}

PTPtr<std::string> Parser::parseExpression() {
    PTPtr<std::string> expressionNode =
        std::make_shared<PTNode<std::string>>("expression");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
    return expressionNode;
}

PTPtr<std::string> Parser::parseTerm() {
    PTPtr<std::string> termNode = std::make_shared<PTNode<std::string>>("term");
    termNode->addChild(this->parseFactor());
    this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);
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
