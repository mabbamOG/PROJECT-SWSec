// !!! all input data assumed to be valid
// !!! recursion has been avoided in all cases


#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "sortedcontainer.h"

int data_compare(data* d1, data* d2)
{
    //assert(d1);
    //assert(d2);
    if(d1->age < d2->age) return -1;
    if(d1->age > d2->age) return 1;
    return strcmp(d1->name, d2->name);
}

// Do not change
void data_print(data* d, FILE* f) 
    {fprintf(f, "%i %s", d->age, d->name);}

data* data_new(int age, char const* name) 
{
    data* d = (data*)malloc(sizeof(data));
    if (d) 
    {
        d->age = age;
        strncpy(d->name, name, NAME_LENGTH-1);
        d->name[NAME_LENGTH-1] = '\0';
    }
    return d;
}

void data_delete(data* d) 
    {free(d);}

node* node_new(data* d) 
{
    node* n = (node*)malloc(sizeof(node));
    if (n)
    {
        n->data = d;
        n->left = NULL;
        n->right = NULL;
    }
    return n;
}

void node_delete(node* n)
{
    data_delete(n->data);
    free(n);
}

sortedcontainer* sortedcontainer_new() 
{
    sortedcontainer* d = (sortedcontainer*)malloc(sizeof(sortedcontainer));
    if (d)
        d->root = NULL;
    return d;
}

/** Structure used for sortedcontainer_index. */
struct index { node* node; node** parent;};

/**
 * @brief finds a node containing @c data
 * @param sc The sorted container to look in
 * @param data The data to look for
 * @return an index struct, containing a node address
 * if one is found, and its "parent" (i.e. the pointer to
 * the memory where node's pointer is stored).
 * If it is not found, node is the latest NULL leaf in
 * the tree where data is supposed to go.
 */
static struct index sortedcontainer_index(sortedcontainer*sc, data* data)
{
    // initialize parent with the memory where the root pointer is
    node** parent = &(sc->root);
    while(*parent)
    {
        // keep looking for a perfect match
        int tmp = data_compare(data, (*parent)->data);
        if (!tmp) // match found
            return (struct index) {.node = *parent, .parent = parent};

        // update parent with left or right node's pointer address.
        parent = (tmp<0 ? &((*parent)->left) : &((*parent)->right));
    }
    // free spot = no match
    return (struct index) {.node = *parent, .parent = parent};
}


void sortedcontainer_insert(sortedcontainer* sc, data* data) 
{
    struct index tmp = sortedcontainer_index(sc, data);

    // if there is a match, do nothing
    // data must also be deleted due to ownership claim
    if (tmp.node)
    {
        data_delete(data);
        return;
    }

    // insert n where tmp.node was previously NULL
    // by using its "parent" pointer
    node* n = node_new(data);
    if (n)
        *(tmp.parent) = n;
    else
        data_delete(data);
}

int sortedcontainer_erase(sortedcontainer* sc, data* data) {
    struct index tmp = sortedcontainer_index(sc, data);

    // if not found, exit
    if(!tmp.node)
        return 0;

    // pointer switcharoo, for convenience
    node* left = tmp.node->left;
    node* right = tmp.node->right;

    // assume one branch is NULL
    *tmp.parent = (left? left: right);

    // if both branches exist, choose LEFT and then
    // move RIGHT to the right-most available leaf
    if (left && right)
    {
        *tmp.parent = left;

        // find a free spot for "right", in the "left" subtree
        // "parent" must be derefenced, just like in function
        // sortedcontainer_index()
        node** parent = &(tmp.node->left->right);
        while(*parent)
            parent = &((*parent)->right);

        *parent = right;
    }
    node_delete(tmp.node);
    return 1;
}

int sortedcontainer_contains(sortedcontainer* sc, data* data) 
    // if there was no match, index.node is NULL
    {return (sortedcontainer_index(sc, data).node? 1: 0);}

// Do not change
static void node_printtree(node* n, int level, FILE* printFile) 
{
    fprintf(printFile, "%*s ", level, "");
    if(n) 
    {
        data_print(n->data, printFile);
        fprintf(printFile, "\n");
        node_printtree(n->left, level+1, printFile);
        node_printtree(n->right, level+1, printFile);
    } else 
        fprintf(printFile, "(nil)\n");
}

// Do not change
void sortedcontainer_print(sortedcontainer* sc, FILE* printFile)
    {node_printtree(sc->root, 0, printFile);}

static void node_deletetree(node* n) 
{
    node* left = n->left;
    node* right = n->right;
    node_delete(n);
    if (left)
        node_deletetree(left);
    if (right)
        node_deletetree(right);
}

void sortedcontainer_delete(sortedcontainer* sc) 
{
    if (sc->root)
        node_deletetree(sc->root);
    free(sc);
}
