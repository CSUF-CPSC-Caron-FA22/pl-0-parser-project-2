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
    PTPtr<std::string> constDeclNode =
        std::make_shared<PTNode<std::string>>("const_declaration");
    PTPtr<std::string> varDeclNode =
        std::make_shared<PTNode<std::string>>("var_declaration");
    PTPtr<std::string> procedure =
        std::make_shared<PTNode<std::string>>("procedure");
    PTPtr<std::string> statementNode =
        std::make_shared<PTNode<std::string>>("statement");

    token_t token;
    token = this->peekNextToken();

    /* Checks if "const" keyword is present in token.
       If the condition is true, then the program will go through
       the token stream stored until it reaches a semicolon (";").*/
    if (token.type == CONST_KEYWORD) {
      constDeclNode->addChild(this->parseConstDeclarationList());
      this->tryMatchTerminal(this->getNextToken(), SEMICOLON, constDeclNode);
    }

    /* Checks if "var" keyword is present in token.
       If the condition is true, then the program will go through the
       token stream stored until it reaches a semicolon (";").*/
    if (token.type == VAR_KEYWORD) {
      varDeclNode->addChild(this->parseVarDeclarationList());
      this->tryMatchTerminal(this->getNextToken(), {SEMICOLON, COMMA}, varDeclNode);
    }

    while (token.type == PROCEDURE_KEYWORD) {
      procedure->addChild(this->parseStatement());
      this->tryMatchTerminal(this->getNextToken(), SEMICOLON, procedure);
    }

    statementNode->addChild(this->parseCondition());
    this->tryMatchTerminal(this->getNextToken(), SEMICOLON, statementNode);

    return blockNode;
}

PTPtr<std::string> Parser::parseConstDeclarations() {
    PTPtr<std::string> constDeclNode =
        std::make_shared<PTNode<std::string>>("const_declaration");
    PTPtr<std::string> constDeclListNode =
        std::make_shared<PTNode<std::string>>("const_declaration_list");

    token_t token;
    token = this->peekNextToken();

    constDeclNode = std::make_shared<PTNode<std::string>>("const");
    constDeclNode->addChild(this->parseConstDeclarationList());
    this->tryMatchTerminal(this->getNextToken(), IDENTIFIER, constDeclNode);

    constDeclNode = std::make_shared<PTNode<std::string>>(token.lexeme);
    constDeclNode->addChild(this->parseConstDeclarationList());
    this->tryMatchTerminal(this->getNextToken(), EQUALS, constDeclNode);

    constDeclNode = std::make_shared<PTNode<std::string>>(token.lexeme);
    constDeclNode->addChild(this->parseConstDeclarationList());
    this->tryMatchTerminal(this->getNextToken(), SEMICOLON, constDeclNode);

    return constDeclNode;
}

PTPtr<std::string> Parser::parseConstDeclarationList() {
    PTPtr<std::string> constDeclListNode =
        std::make_shared<PTNode<std::string>>("const_declaration_list");

    token_t token;
    token = this->peekNextToken();

    if (token.type == CONST_KEYWORD) {
      while(true) {

        constDeclListNode = std::make_shared<PTNode<std::string>>(",");
        constDeclListNode->addChild(this->parseVarDeclarations());

        constDeclListNode = std::make_shared<PTNode<std::string>>(token.lexeme);
        constDeclListNode->addChild(this->parseVarDeclarations());
        //this->tryMatchTerminal(this->getNextToken(), EQUALS, constDeclListNode);
        this->tryMatchTerminal(this->getNextToken(), {CONST_KEYWORD, IDENTIFIER, EQUALS, SEMICOLON, NUMBER_LITERAL}, constDeclListNode);

        constDeclListNode = std::make_shared<PTNode<std::string>>(token.lexeme);
        constDeclListNode->addChild(this->parseVarDeclarations());
        //this->tryMatchTerminal(this->getNextToken(), SEMICOLON, constDeclListNode);
        this->tryMatchTerminal(this->getNextToken(), {CONST_KEYWORD, IDENTIFIER, EQUALS, SEMICOLON, NUMBER_LITERAL}, constDeclListNode);
        }
      }

    /* constDeclListNode->addChild(this->parseVarDeclarations());
    this->tryMatchTerminal(this->getNextToken(), NUMBER_LITERAL, constDeclListNode); */

    return constDeclListNode;
}

PTPtr<std::string> Parser::parseVarDeclarations() {
    PTPtr<std::string> varDeclNode =
        std::make_shared<PTNode<std::string>>("var_declaration");
    PTPtr<std::string> varDeclListNode =
        std::make_shared<PTNode<std::string>>("var_declaration_list");

    token_t token;
    token = this->peekNextToken();

    varDeclNode = std::make_shared<PTNode<std::string>>("var");
    varDeclNode->addChild(this->parseVarDeclarationList());
    this->tryMatchTerminal(this->getNextToken(), IDENTIFIER, varDeclNode);

    varDeclListNode = std::make_shared<PTNode<std::string>>(token.lexeme);
    varDeclListNode->addChild(this->parseProcedure());
    this->tryMatchTerminal(this->getNextToken(), SEMICOLON, varDeclListNode);

    return varDeclNode;
}

PTPtr<std::string> Parser::parseVarDeclarationList() {
    PTPtr<std::string> varDeclNodeList =
        std::make_shared<PTNode<std::string>>("var_declaration_list");

    token_t token;
    token = this->peekNextToken();

    if (token.type == VAR_KEYWORD) {
      while(true) {

        varDeclNodeList = std::make_shared<PTNode<std::string>>(token.lexeme);
        varDeclNodeList->addChild(this->parseProcedure());
        this->tryMatchTerminal(this->getNextToken(), COMMA, varDeclNodeList);

        varDeclNodeList = std::make_shared<PTNode<std::string>>(",");
        varDeclNodeList->addChild(this->parseProcedure());
        this->tryMatchTerminal(this->getNextToken(), IDENTIFIER, varDeclNodeList);

        }
      }

    return varDeclNodeList;
}

PTPtr<std::string> Parser::parseProcedure() {
    PTPtr<std::string> procedure =
        std::make_shared<PTNode<std::string>>("procedure");
    PTPtr<std::string> blockNode = this->parseBlock();

    token_t token;
    token = this->peekNextToken();

    /* If the "procedure" keyword is present, then the program will
    add the identifier and block to the tree until it reaches a semicolon. */
    while (token.type == PROCEDURE_KEYWORD) {
    //  procedure = std::make_shared<PTNode<std::string>>(token.lexeme);
      procedure->addChild(this->parseStatement());
      //this->tryMatchTerminal(this->getNextToken(), SEMICOLON, procedure);

      blockNode->addChild(this->parseStatement());
      this->tryMatchTerminal(this->getNextToken(), SEMICOLON, blockNode);
    }

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

    if (token.type == IDENTIFIER) {
      expressionNode = std::make_shared<PTNode<std::string>>(token.lexeme);

      parseExpression();
      expressionNode->addChild(this->parseTerm());
      this->tryMatchTerminal(this->getNextToken(), DEFINE_EQUALS, expressionNode);
    }
    else if (token.type == CALL_KEYWORD) {
      statementNode = std::make_shared<PTNode<std::string>>("call");
      statementNode->addChild(this->parseCondition());
      this->tryMatchTerminal(this->getNextToken(), IDENTIFIER, statementNode);
    }
    else if (token.lexeme.compare("?") == 0) {
      expressionNode = std::make_shared<PTNode<std::string>>("?");
      expressionNode->addChild(this->parseTerm());
      this->tryMatchTerminal(this->getNextToken(), IDENTIFIER, statementNode);
    }
    else if (token.lexeme.compare("!") == 0) {
      statementNode = std::make_shared<PTNode<std::string>>("!");
      statementNode->addChild(this->parseCondition());
      this->tryMatchTerminal(this->getNextToken(), IDENTIFIER, statementNode);
    }
    else if (token.type == BEGIN_KEYWORD) {
      statementNode = std::make_shared<PTNode<std::string>>("begin");
      statementNode->addChild(this->parseCondition());

      while (token.type == SEMICOLON) {
        statementNode = std::make_shared<PTNode<std::string>>(";");
        statementNode->addChild(this->parseCondition());
      }

      this->tryMatchTerminal(this->getNextToken(), END_KEYWORD, statementNode);
    }
    else if (token.type == IF_KEYWORD) {
      conditionNode = std::make_shared<PTNode<std::string>>("if");
      conditionNode->addChild(this->parseExpression());
      this->tryMatchTerminal(this->getNextToken(), THEN_KEYWORD, conditionNode);

      statementNode = std::make_shared<PTNode<std::string>>("then");
      statementNode->addChild(this->parseCondition());
      this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);
    }
    else if (token.type == WHILE_KEYWORD) {
      conditionNode = std::make_shared<PTNode<std::string>>("while");
      conditionNode->addChild(this->parseExpression());
      this->tryMatchTerminal(this->getNextToken(), DO_KEYWORD, conditionNode);

      statementNode = std::make_shared<PTNode<std::string>>("do");
      statementNode->addChild(this->parseCondition());
      this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);
    }

    //this->tryMatchTerminal(token, {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER, END_KEYWORD}, statementNode);

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
      if (token.type == EQUALS) {
        expressionNode = std::make_shared<PTNode<std::string>>("=");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
      }
      else if (token.lexeme.compare("<") == 0) {
        expressionNode = std::make_shared<PTNode<std::string>>("<");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
      }
      else if (token.lexeme.compare("<=") == 0) {
        expressionNode = std::make_shared<PTNode<std::string>>("<=");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
      }
      else if (token.lexeme.compare(">") == 0) {
        expressionNode = std::make_shared<PTNode<std::string>>(">");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
      }
      else if (token.lexeme.compare(">=") == 0) {
        expressionNode = std::make_shared<PTNode<std::string>>(">=");
        expressionNode->addChild(this->parseTerm());
        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);
      }
    }

    /*conditionNode->addChild(this->parseExpression());
    this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, conditionNode);*/

    return conditionNode;
}

PTPtr<std::string> Parser::parseExpression() {
    PTPtr<std::string> expressionNode =
        std::make_shared<PTNode<std::string>>("expression");
    PTPtr<std::string> termNode = std::make_shared<PTNode<std::string>>("term");

    token_t token;
    token = this->peekNextToken();

    if (token.type == ADD_OP) {
      termNode = std::make_shared<PTNode<std::string>>("+");
      termNode->addChild(this->parseFactor());
      this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);
    }
    else if (token.lexeme.compare("-") == 0) {
      termNode = std::make_shared<PTNode<std::string>>("-");
      termNode->addChild(this->parseFactor());
      this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);
    }

    // expressionNode->addChild(this->parseTerm());
    // this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, expressionNode);

    return expressionNode;
}

PTPtr<std::string> Parser::parseTerm() {
    PTPtr<std::string> termNode = std::make_shared<PTNode<std::string>>("term");
    PTPtr<std::string> factorNode =
        std::make_shared<PTNode<std::string>>("factor");

    token_t token;
    token = this->peekNextToken();

    termNode->addChild(this->parseFactor());
    this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);

    while (token.lexeme.compare("factor") == 0) {
      if (token.type == MUL_OP) {
        factorNode = std::make_shared<PTNode<std::string>>("*");
        factorNode->addChild(this->parseFactor());
        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, factorNode);
      }

      else if (token.lexeme.compare("/") == 0) {
        factorNode = std::make_shared<PTNode<std::string>>("/");
        termNode->addChild(this->parseFactor());
        this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, termNode);
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
    PTPtr<std::string> expressionNode =
        std::make_shared<PTNode<std::string>>("expression");

    token_t token;
    token = this->peekNextToken();

    if (token.type == IDENTIFIER) {
      factorNode = std::make_shared<PTNode<std::string>>(token.lexeme);
      this->tryMatchTerminal(this->getNextToken(), SEMICOLON, factorNode);
    }
    else if (token.type == NUMBER_LITERAL) {
      factorNode = std::make_shared<PTNode<std::string>>(token.lexeme);
      this->tryMatchTerminal(this->getNextToken(), SEMICOLON, factorNode);
    }
    else if (token.type == LEFT_PAREN) {
      expressionNode = std::make_shared<PTNode<std::string>>("(");
      expressionNode->addChild(this->parseTerm());
      this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, SEMICOLON}, expressionNode);
    }

    //this->tryMatchTerminal(this->getNextToken(), {RIGHT_PAREN, NUMBER_LITERAL, IDENTIFIER}, factorNode);

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
