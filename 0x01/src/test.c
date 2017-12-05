#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "test.h"
#include "sortedcontainer.h"

typedef int (*test_t)(FILE* );

#define ASSERT(c,s) do { \
    if(!(c)) { \
        fprintf(printFile, "ASSERT FAILED[%s:%i] %s: %s\n", __FILE__, __LINE__, #c, s); \
        return 1; \
    } \
} while(0)

int test1(FILE* printFile) { // new
    (void)printFile;
    sortedcontainer* sc = sortedcontainer_new();
    ASSERT(sc != NULL, "failed to create sorted container");
    ASSERT(sc->root == NULL, "root is not NULL");
    sortedcontainer_delete(sc);
    return 0;
}

int test2(FILE* printFile) { // insert + erase + contains + (data_compare) + (data_new)
    (void)printFile;
    sortedcontainer* sc = sortedcontainer_new();
    ASSERT(sc != NULL, "failed to create sorted container");
    ASSERT(sc->root == NULL, "root is not NULL");

    data* dat = data_new(10, "aap");
    sortedcontainer_insert(sc, dat);
    ASSERT(sc->root != NULL, "root is NULL");
    ASSERT(sc->root->data != NULL, "root->data is NULL");
    ASSERT(!data_compare(dat, sc->root->data), "data is not equivalent");
    ASSERT(dat == sc->root->data, "data is not the same instant");
    ASSERT(sc->root->left == NULL, "root->data->left is not NULL");
    ASSERT(sc->root->right == NULL, "root->data->right is not NULL");

    data* dataDifferent = data_new(20, "noot");
    data* dataCopy = data_new(10, "aap");
    ASSERT(sortedcontainer_contains(sc, dataCopy), "data is not in the container (sortedcontainer_contains)");
    ASSERT(!sortedcontainer_contains(sc, dataDifferent), "data should not be in the container");

    sortedcontainer_erase(sc, dataDifferent);
    ASSERT(sc->root->data != NULL, "data wrongly deleted");
    ASSERT(!data_compare(dat, sc->root->data), "data wrongly deleted");
    ASSERT(dat == sc->root->data, "data wrongly deleted");
    ASSERT(sortedcontainer_contains(sc, dataCopy), "data is not in the container anymore (sortedcontainer_contains)");
    ASSERT(!sortedcontainer_contains(sc, dataDifferent), "data should not be in the container");

    sortedcontainer_erase(sc, dataCopy);
    ASSERT(sc->root == NULL, "data not deleted");

    sortedcontainer_delete(sc);
    data_delete(dataDifferent);
    data_delete(dataCopy);
    return 0;
}

int test3(FILE* printFile) { // like previous, but more nodes
    (void)printFile;
    sortedcontainer* sc = sortedcontainer_new();

    data* aap = data_new(10, "aap");
    data* noot = data_new(20, "noot");
    data* mies = data_new(15, "mies");
    sortedcontainer_insert(sc, aap);
    sortedcontainer_insert(sc, noot);
    sortedcontainer_insert(sc, mies);
    ASSERT(sc != NULL, "failed to create sorted container");
    ASSERT(sc->root != NULL, "root is NULL");
    ASSERT(sc->root->data != NULL, "root->data is NULL");
    ASSERT(!data_compare(aap, sc->root->data), "data is not equivalent");
    ASSERT(aap == sc->root->data, "data is not the same instant");
    ASSERT(!data_compare(noot, sc->root->right->data), "data is not equivalent");
    ASSERT(noot == sc->root->right->data, "data is not the same instant");
    ASSERT(!data_compare(mies, sc->root->right->left->data), "data is not equivalent");
    ASSERT(mies == sc->root->right->left->data, "data is not the same instant");

    sortedcontainer_erase(sc, noot);
    ASSERT(!data_compare(aap, sc->root->data), "data is not equivalent");
    ASSERT(aap == sc->root->data, "data is not the same instant");
    ASSERT(!data_compare(mies, sc->root->right->data), "data is not deleted");
    ASSERT(mies == sc->root->right->data, "data is not deleted");
    ASSERT(sc->root->right->left == NULL, "left child of mies' node is not NULL");
    ASSERT(sc->root->right->right == NULL, "right child of mies' node is not NULL");
    ASSERT(sortedcontainer_contains(sc, aap), "data is not in the container anymore (sortedcontainer_contains)");
    ASSERT(sortedcontainer_contains(sc, mies), "data is not in the container anymore (sortedcontainer_contains)");

    sortedcontainer_delete(sc);
    return 0;
}

int test4(FILE* printFile) {
    (void)printFile;

    // Add a test in the style of test3 to test the deletion of a node with
    // two children
    sortedcontainer* sc = sortedcontainer_new();
    data* Ten = data_new(10, "Ten");
    data* Fifteen = data_new(15, "Fifteen");
    data* Five = data_new(5, "Five");
    data* Three = data_new(3, "Three");
    data* Sixteen = data_new(16, "Sixteen");
    ASSERT(sc != NULL, "sortedcontainer failed to initalize");
    ASSERT(Ten != NULL, "data failed to initialize");
    ASSERT(Fifteen != NULL, "data failed to initialize");
    ASSERT(Five != NULL, "data failed to initialize");
    ASSERT(Three != NULL, "data failed to initialize");

    sortedcontainer_insert(sc, Ten);
    sortedcontainer_insert(sc, Fifteen);
    sortedcontainer_insert(sc, Five);
    sortedcontainer_insert(sc, Three);
    sortedcontainer_insert(sc, Sixteen);
    ASSERT(sortedcontainer_contains(sc, Ten) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Five) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Fifteen) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Three) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Sixteen) == 1, "data missing from container");
    ASSERT(Ten == sc->root->data, "data missing from container");
    ASSERT(Five == sc->root->left->data, "data missing from container");
    ASSERT(Fifteen == sc->root->right->data, "data missing from container");
    ASSERT(Three == sc->root->left->left->data, "data missing from container");
    ASSERT(Sixteen == sc->root->right->right->data, "data missing from container");
    ASSERT(data_compare(Ten, sc->root->data) == 0, "wrong data in th container");
    ASSERT(data_compare(Five, sc->root->left->data) == 0, "wrong data in the container");
    ASSERT(data_compare(Fifteen, sc->root->right->data) == 0, "wrong data in the container");
    ASSERT(data_compare(Three, sc->root->left->left->data) == 0, "data missing from container");
    ASSERT(data_compare(Sixteen, sc->root->right->right->data) == 0, "data missing from container");

    data* Ten_Copy = data_new(10, "Ten");
    ASSERT(Ten_Copy != NULL, "data init error");
    ASSERT(sortedcontainer_erase(sc, Ten) == 1, "no node deleted!");
    ASSERT(sortedcontainer_erase(sc, Ten_Copy) == 0, "invalid node deleted!");
    ASSERT(sc->root != NULL, "missing root");
    ASSERT(sc->root->data != NULL, "missing root");
    ASSERT(sc->root->left != NULL, "missing left child");
    ASSERT(sc->root->right != NULL, "missing right child");
    ASSERT(sc->root->left->left == NULL, "missing right child");
    ASSERT(sc->root->left->right == NULL, "missing right child");
    ASSERT(sc->root->right->right != NULL, "missing right child");
    ASSERT(sc->root->right->left == NULL, "missing right child");
    ASSERT(sc->root->left->data == Three, "left child not Three");
    ASSERT(sc->root->right->data == Fifteen, "right child not Six");
    ASSERT(sc->root->right->right->data == Sixteen, "right child not Six");

    data_delete(Ten_Copy);
    sortedcontainer_delete(sc);
    return 0;
}

int test5(FILE* printFile) {
    (void)printFile;

    // Add a test in the style of test3 to test the deletion of a node with
    // two children, who each have two children as well
    sortedcontainer* sc = sortedcontainer_new();
    data* Ten = data_new(10, "Ten");
    data* Fifteen = data_new(15, "Fifteen");
    data* Five = data_new(5, "Five");
    data* Three = data_new(3, "Three");
    data* Six = data_new(6, "Six");
    data* Seven = data_new(7, "Seven");
    data* Eight = data_new(8, "Eight");
    data* Nine = data_new(9, "Nine");
    data* Fourteen = data_new(14, "Fourteen");
    data* Sixteen = data_new(16, "Sixteen");
    ASSERT(sc != NULL, "sortedcontainer failed to initalize");
    ASSERT(Ten != NULL, "data failed to initialize");
    ASSERT(Fifteen != NULL, "data failed to initialize");
    ASSERT(Five != NULL, "data failed to initialize");
    ASSERT(Three != NULL, "data failed to initialize");
    ASSERT(Six != NULL, "data failed to initialize");
    ASSERT(Seven != NULL, "data failed to initialize");
    ASSERT(Eight != NULL, "data failed to initialize");
    ASSERT(Nine != NULL, "data failed to initialize");

    sortedcontainer_insert(sc, Ten);
    sortedcontainer_insert(sc, Fifteen);
    sortedcontainer_insert(sc, Five);
    ASSERT(sortedcontainer_contains(sc, Ten) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Five) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Fifteen) == 1, "data missing from container");
    ASSERT(Ten == sc->root->data, "data missing from container");
    ASSERT(Five == sc->root->left->data, "data missing from container");
    ASSERT(Fifteen == sc->root->right->data, "data missing from container");
    ASSERT(data_compare(Ten, sc->root->data) == 0, "wrong data in th container");
    ASSERT(data_compare(Five, sc->root->left->data) == 0, "wrong data in the container");
    ASSERT(data_compare(Fifteen, sc->root->right->data) == 0, "wrong data in the container");

    sortedcontainer_insert(sc, Three);
    sortedcontainer_insert(sc, Six);
    sortedcontainer_insert(sc, Seven);
    sortedcontainer_insert(sc, Eight);
    sortedcontainer_insert(sc, Nine);
    sortedcontainer_insert(sc, Fourteen);
    sortedcontainer_insert(sc, Sixteen);
    ASSERT(sortedcontainer_contains(sc, Three) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Six) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Seven) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Eight) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Nine) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Fourteen) == 1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Sixteen) == 1, "data missing from container");

    data* Ten_Copy = data_new(10, "Ten");
    ASSERT(Ten_Copy != NULL, "data init error");
    ASSERT(sortedcontainer_erase(sc, Ten) == 1, "no node deleted!");
    ASSERT(sortedcontainer_erase(sc, Ten_Copy) == 0, "invalid node deleted!");
    ASSERT(sc->root != NULL, "missing root");
    ASSERT(sc->root->data != NULL, "missing root");
    ASSERT(sc->root->left != NULL, "missing left child");
    ASSERT(sc->root->right != NULL, "missing right child");
    ASSERT(sc->root->left->data == Three, "left child not Three");
    ASSERT(sc->root->right->data == Six, "right child not Six");
    ASSERT(sc->root->right->right->right->right->right->data == Fifteen, "Fifteen not in place");
    ASSERT(sortedcontainer_contains(sc, Fourteen)==1, "data missing from container");
    ASSERT(sortedcontainer_contains(sc, Sixteen) == 1, "data missing from container");

    data_delete(Ten_Copy);
    sortedcontainer_delete(sc);
    return 0;
}

// If you want to add test6 and onwards, create the test6 function above and
// add it to this list
test_t tests[] = {test1, test2, test3, test4, test5};

void test(FILE* printFile) {
    fprintf(printFile, "Testing...\n");
    int max = sizeof(tests)/sizeof(*tests);
    int lmax = 1 + log10(max);
    for(int i = 0; i < max; ++i) {
        int r = tests[i](printFile);
        fprintf(printFile, "[%*i/%i] ", lmax, i+1, max);
        if(r) {
            fprintf(printFile, "FAIL\n");
        } else {
            fprintf(printFile, "PASS\n");
        }
    }
}
