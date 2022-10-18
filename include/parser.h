/**
 *  CPSC 323 Compilers and Languages
 * 
 *  Dalton Caron, Teaching Associate
 *  dcaron@fullerton.edu, +1 949-616-2699
 *  Department of Computer Science
 */
#ifndef PARSER_H__
#define PARSER_H__

#include <lexer.h>
#include <stack>
#include <initializer_list>
#include <parse_tree.h>

class Parser {
public:
    /**
     * Parser constructor consumes the token stream outputted from the lexical 
     * analyzer.
     * 
     * @param tokens A stream of tokens consituting the tokenized source code 
     * input.
     */
    Parser(const token_stream_t &tokens);
    virtual ~Parser();

    /**
     * Parses the program source code and stores the result as a parse tree.
     * 
     * @return A parse tree of the source code.
     */
    ParseTree<std::string> parse();
private:
    /**
     * Fetches the next token from the token stream and increments the token 
     * stream marker forward by 1 place.
     * 
     * @return The next token from the token stream.
     */
    token_t getNextToken();

    /**
     * Fetches the next token from the token stream without incrementing the 
     * token stream marker.
     * 
     * @return The next token from the token stream.
     */
    token_t peekNextToken() const;

    /**
     * Each of these parsing functions are derived from the grammar.
     * @return The node representing the production with the non-terminal 
     * being the root node of the tree.
     */
    PTPtr<std::string> parseProgram();
    PTPtr<std::string> parseBlock();
    PTPtr<std::string> parseConstDeclarations();
    PTPtr<std::string> parseConstDeclarationList();
    PTPtr<std::string> parseVarDeclarations();
    PTPtr<std::string> parseVarDeclarationList();
    PTPtr<std::string> parseProcedure();
    PTPtr<std::string> parseStatement();
    PTPtr<std::string> parseCondition();
    PTPtr<std::string> parseExpression();
    PTPtr<std::string> parseTerm();
    PTPtr<std::string> parseFactor();

    /**
     * Utility function for attempting to consume a terminal from the token 
     * stream stored in the Parser. If it fails, an error is thrown and the 
     * program exits.
     * 
     * @param actual Token from the input to test.
     * @param expected The class the token from the input is expected to have.
     * @param node The node to insert the consumed terminal as a child of.
     */
    void tryMatchTerminal(
        const token_t &actual, 
        const token_class_t expected,
        PTPtr<std::string> node
    ) const;

    /**
     * Utility function for attempting to consume a terminal from the token 
     * stream stored in the Parser. If it fails, an error is thrown and the 
     * program exits.
     * 
     * @param actual Token from the input to test.
     * @param expected The classes the token from the input is expected to be.
     * @param node The node to insert the consumed terminal as a child of.
     */
    void tryMatchTerminal(
        const token_t &actual, 
        const std::initializer_list<token_class_t> expected,
        PTPtr<std::string> node
    ) const;

    /**
     * Utility function for raising an error when the token obtained was not 
     * the class expected. Terminates the program.
     * 
     * @param actual The token instance obtained.
     * @param expected The token class that was desired.
     */
    void raiseMismatchError(
        const token_t &actual, 
        const token_class_t expected
    ) const;

    /**
     * Utility function for raising an error when the token obtained was not 
     * the class expected. Terminates the program.
     * 
     * @param actual The token instance obtained.
     * @param expected The token classes that were desired.
     */
    void raiseMismatchError(
        const token_t &actual, 
        const std::initializer_list<token_class_t> expected
    ) const;

    // Token stream input from the lexical analyzer.
    const token_stream_t tokens;
    // Current location within the token stream input.
    unsigned int current_token = 0;
};

#endif