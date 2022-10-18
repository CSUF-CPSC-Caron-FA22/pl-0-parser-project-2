/**
 *  CPSC 323 Compilers and Languages
 * 
 *  Dalton Caron, Teaching Associate
 *  dcaron@fullerton.edu, +1 949-616-2699
 *  Department of Computer Science
 */
#ifndef PARSE_TREE_H__
#define PARSE_TREE_H__

#include <string>
#include <memory>
#include <functional>
#include <vector>

// Forward declaration for alias.
template<class T>
class PTNode;

// Parse Tree Pointer.
template<class T>
using PTPtr = std::shared_ptr<PTNode<T>>;

template<class T>
using ParseTree = PTPtr<T>;

// Parse Tree Node.
template<class T>
class PTNode {
public:
    PTNode(const T value);
    virtual ~PTNode();

    /**
     * Perfomrs a DFS traversal of the tree. Performs a user-provided action 
     * on each of the tree node values in the DFS order.
     * 
     * @param root The root node of the tree.
     * @param action The function to perform on the values held in each node.
     */
    static void dfsTraversal(
        PTPtr<T> root, 
        const std::function<void(T)> action
    );

    /**
     * Prints an ASCII-console representation of the tree.
     * @param root Root node of the tree to print.
     * @param treeSize Requires the size of the tree. Over estimate the number 
     * if it is unknown.
     */
    static void printTree(PTPtr<T> root, int treeSize);

    /**
     * @return Value associated with this node.
     */
    T getValue() const;

    /**
     * @return The children of this node.
     */
    const std::vector<PTPtr<T>> &getChildren() const;

    /**
     * Makes the provided node a child node of this tree.
     * @param node Node to child.
     */
    PTPtr<T> addChild(const PTPtr<T> node);

    /**
     * Makes the value into a node and child node of this tree.
     * @param value Value to turn into a node and child.
     */
    PTPtr<T> addChild(const T value);
private:
    static void printNTree(
        PTPtr<T> node,
        std::vector<bool> flag, 
        int depth = 0,
        bool isLast = false
    );

    T value;
    std::vector<PTPtr<T>> children;
};

#include <parse_tree.cpp>
#endif