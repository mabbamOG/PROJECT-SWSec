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
 *
 * PROBLEMS FOUND:
 * 1) there is no check for age>0. this has been fixed with a new scanf() format string
 * 2) there is no check for integer overflow
 * 2) the last "%s" in the scanf() allowed for a buffer overflow. 
 *    This has been fixed to the current NAME_LENGTH-1 (since scanf adds \0 automatically)
 *    While this value is embedded into the string literal, it is possible to use a macro (stringize) to make this string parametrized.
 *    The macro has been provided below.
 * 3) the return value for data_new() was not checked for success!
 *
 * SOLUTION:
 * 1) A more robust scanf() format string has been used, to remain loyal to the original code structure.
 *   The specification is interpreted as the following (extended) regex: ^[iec]\s+[0-9]{1,10}\s+[a-zA-Z]{1,19}\s*$
 * 2) Scanf() is checked for errors such as correct amount of fields interpreted, strings too large, excess garbage (e.g. byte injections).
 * 3) The Integer is checked for overflow. Due to scanf()'s fragility, a "trick" had to be used: the integer is processed as a float
 *    number so that it may be safely checked against for overflows (floats default to inf) without losing precision 
 *    (a long double is used for a 10-digit number). In order to prevent users from entering any kind of float, 
 *    the input is processed as an array of digits.
 */
data* read_data(char const* command) 
{
    // initialized variables
    int age = 0;
    char name[NAME_LENGTH] = {'\0'};
    long double d = 0.0;
    char dp[11] = {'\0'};
    int n = 0;

    // '%': indicates a field to be parsed and matched against
    // ' ': indicates any amount of whitespace. is a special kind of field, not beginning with %
    //
    // FLAGS
    // '*': discards the following match
    // <num>: indicates maximum number of occurrences for the match
    //
    // CONVERSIONS
    // '[a-z]': indicates a range of possible consecutive characters that form a string. the string will be null terminated
    // 'n': stores the amount of characters parsed so far into a variable. is used for checking there is no garbage left at string ending.
    int name_len = NAME_LENGTH -1;
    int int_chr_len = 0;
    char* format_str = "%*1[iec]%*1[ ] %10[0-9]%*1[ ] %19[a-zA-Z] %n"
    int i = sscanf(command, , dp, name,  &n);

    // check for scanf() errors, correct amount of read fields, and lack of unexpected values left in the string.
    if (i==EOF || errno!=0 || i!=2 || strlen(command)-n != 0)
        return NULL;

    // converts the integer to a floating point number to check for overflows, and then casts to int. no precision is lost.
    sscanf(dp, "%Lf", &d);
    age = (int) d;
    if (d>INT_MAX || d<1)
        return NULL;

    return data_new(age, name);
}

/**
 * @brief prints "Invalid input\n" to @c printFile stream, as per program specification
 * this function is useful for cleaner code in handle_command() function.
 * @param printFile FILE to print messages to
 * @return 0, always
 */
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
 *
 * PROBLEMS FOUND:
 * 1) There was a possible data leak in the fprintf(printFile, command). This is because an attacker might place
 *    a pre-formatted string in @c command and pop/leak values from the stack.
 * 2) Read_data(data) was never checked for errors, this could've resulted in passing a NULL value
 *    to functions that do not expect one.
 * 3) Out of the functions that require initializing data, only sortedcontainer_insert claims ownership of data and will deallocate it.
 *
 *  SOLUTIONS:
 * 1) Use preformatted strings in all fprintf() commands, to avoid injections.
 * 2) Always check for allocation errors.
 * 3) Always remember to deallocate data that is no longer used, to prevent memory leaks.
 *
 */
int handle_command(FILE* printFile, sortedcontainer* sc, char* command) 
{
    switch(*command) 
    {
    case 'i':
        {
        // if data cannot be initialized, do nothing
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
        delete_data(data);
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
        delete_data(data);
        break;
        }
    case 'p':
        // if the buffer does not contain only one character, do nothing.
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
        fprintf(printFile, "%s", command); // fix data leak
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
        if(fgets(inputAt, incr, in) == NULL) 
        { 
            if (inputAt>input) break; // check that this isn't successive EOF
            free(input); 
            return NULL; 
        } // error: free input
        if(inputAt[incr - 1] != '\0' || inputAt[incr - 2] == '\n')
            break;
        if (inputMaxLength > INT_MAX - INPUT_INCREMENT) // small overflow
        {
            free(input);
            return NULL;
        }
        inputMaxLength += INPUT_INCREMENT;
        char* tmpinput = realloc(input, sizeof(char) * inputMaxLength); // error: check realloc // error: input = realloc not good if fail
        if (!tmpinput)
        {
            free(input);
            return NULL;
        }
        input = tmpinput; // error2 of tmpinput
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
        if(!command)
            break;
        if(handle_command(stdout, sc, command))
        {
            free(command);
            break;
        }
        free(command); // memory leak
    }
    sortedcontainer_delete(sc);
    fprintf(stdout, "\nBye.\n");
    return 0;
}
