#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sortedcontainer.h"
#include "test.h"

#include <errno.h>
#include <limits.h>

// DO NOT change this value. It does not fix your problems
#define INPUT_INCREMENT 10

/**
 * @brief Prints the prompt to @c f
 * @param f The FILE to print the prompt to.
 */
void print_prompt(FILE* f) 
{
    fprintf(f, "\n> "); 
    fflush(f);
}

/**
 * @brief Basic parser to read data from @c command
 * @param command The command string to read the data from
 * @return A new data object
 *
 * TO FIX:
 *   There are two serious problems in this function that are related
 */
data* read_data(char const* command) 
{
    int age;
    char name[NAME_LENGTH];
    long double d = 0.0;
    char dp[11] = {'\0'};
    int n = 0;
    int i = sscanf(command, "%*1[iec]%*1[ ] %10[0-9]%*1[ ] %19[a-zA-Z] %n", dp, name,  &n);
    if (i==EOF || errno!=0 || i!=2 || strlen(command)-n != 0)
        return NULL;
    sscanf(dp, "%Lf", &d);
    age = (int) d;
    if (d>INT_MAX || d<1)
        return NULL;
    return data_new(age, name);
}

int invalid_input(FILE* printFile)
{
    fprintf(printFile, "Invalid input\n");
    return 0;
}

/**
 * @brief Handles @c command
 * @param printFile FILE to print messages to
 * @param sc The sortedcontainer to query or modify
 * @param command The command to handle
 * @return 1 iff the problem should quit, otherwise 0
 *
 * TO FIX:
 *   There are three problems in this function, two of which are related
 */
int handle_command(FILE* printFile, sortedcontainer* sc, char* command) 
{
    switch(*command) 
    {
    case 'i':
        {
        data* data = read_data(command);
        if(!data)
            return invalid_input(printFile);
        sortedcontainer_insert(sc, data);
        break;
        }
    case 'e':
        {
        data* data = read_data(command);
        if(!data)
            return invalid_input(printFile);
        sortedcontainer_erase(sc, data);
        break;
        }
    case 'c':
        {
        data* data = read_data(command);
        if(!data)
            return invalid_input(printFile);
        if(sortedcontainer_contains(sc, data))
            fprintf(printFile, "y\n");
        else
            fprintf(printFile, "n\n");
        break;
        }
    case 'p':
        if (command[1]!='\0')
            return invalid_input(printFile);
        sortedcontainer_print(sc, printFile);
        break;
    case 'x':
        if (command[1]!='\0')
            return invalid_input(printFile);
        return 1;
        break;
    case 't':
        if (command[1]!='\0')
            return invalid_input(printFile);
        test(printFile);
        break;
    default:
        fprintf(printFile, "No such command: ");
        fprintf(printFile, "%s", command); // data leak
        fprintf(printFile, "\n");
        break;
    }
    return 0;
}

/**
 * @brief Reads a command from the FILE @c in
 * @param in FILE to read a command from
 * @return The read command
 *
 * TO FIX:
 *   There are two separate problems in this function. Fix these problems
 *   by only changing TWO lines in total.
 */

char* read_command(FILE* in) {
    int inputMaxLength = 0;
    char* input = NULL;
    char* inputAt = NULL;
    int incr = INPUT_INCREMENT;
    inputMaxLength = incr;
    input = (char*)malloc(sizeof(char) * incr); // error: malloc
    if (!input)
        return NULL;
    inputAt = input;
    do 
    {
        inputAt[incr - 1] = 'e';
        if(fgets(inputAt, incr, in) == NULL) return NULL;
        if(inputAt[incr - 1] != '\0' || inputAt[incr - 2] == '\n')
            break;
        if (inputMaxLength + INPUT_INCREMENT < inputMaxLength) // small overflow
        {
            free(input);
            return NULL;
        }
        inputMaxLength += INPUT_INCREMENT;
        input = realloc(input, sizeof(char) * inputMaxLength); // error: realloc
        if (!input)
        {
            free(input);
            return NULL;
        }
        inputAt = input + inputMaxLength - INPUT_INCREMENT - 1; // error: not relative to input
        incr = INPUT_INCREMENT + 1;
    } while(1);
    input[strlen(input)-1+ (!input[0]? 1:0)] = 0; // error: byte injection
    return input;
}

/**
 * @brief The main SINT loop
 * @param argc Argument count
 * @param argv Arguments
 * @return 0
 *
 * TO FIX:
 *   One issue needs to be fixed here.
 */
int main(int argc, char* argv[]) 
{
    (void)argc; (void) argv;
    sortedcontainer* sc = sortedcontainer_new();
    if (!sc)
        return fprintf(stderr, "\nError initializing container.\n");

    while(1) 
    {
        print_prompt(stdout);
        char* command = read_command(stdin);
        if(command == NULL)
            break;
        if(handle_command(stdout, sc, command))
            break;
        free(command); // memory leak
    }
    sortedcontainer_delete(sc);
    fprintf(stdout, "\nBye.\n");
    return 0;
}
