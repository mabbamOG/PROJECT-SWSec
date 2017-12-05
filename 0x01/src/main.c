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
 * 1) "%i" -> There is no check for age>=0.
 *    It is unclear whether age 0 is to be accepted, but all specification so far suggests otherwise.
 * 2) "%i" -> There is no check for integer overflow.
 *    A change of spec type (for age) to a machine-indepent bit-size would allow avoiding this number.
 *    It would then be possible to use the following amount for digit size (see solution below): 
 *    int digit_size = snprintf(0, NULL, "%d", INT_MAX)
 * 3) "%s" -> The last "%s" in the scanf() allowed for a buffer overflow, due to lack of string size bounding.
 *
 * SOLUTION:
 * 1) A more robust scanf() format string has been used, to remain loyal to the original code structure.
 *   The specification is interpreted as the following (extended) regex: ^[iec]\s+[0-9]{1,10}\s+[a-zA-Z]{1,19}\s*$
 *   We have chosen 10-digit integers as a limit, due to the INT_MAX for 32bit integers.
 *   We have set the name to a limit of NAME_LENGTH-1 (since scanf adds \0 automatically) and also
 *   have limited the alphabet to ascii characters. This is to prevent byte injections, and also
 *   because the specification allows only the storage of char values for name. (thus, unicode is not supported).
 * 2) Scanf() is checked for errors such as correct amount of fields interpreted, strings too large, excess garbage (e.g. byte injections), etc.
 * 3) The Integer is checked for overflow. In an attempt to avoid modifying the original code too much, the scanf() function has been used.
 *    Due to C99 standard overflows have unexpected effect, so they must be prevented.
 *    Since scanf() does not check for them, the only way to prevent them is to store the integer in a variable of larger size (numeric or string).
 *    The C99 long long is capable of storing 11+ digit integers, without overflow.
 *    The variable has been chosen unsigned, to prevent the insertion of negative ages ("%Lu").
 */
data* read_data(char const* command) 
{
    // initialized variables
    int age = 0;
    char name[NAME_LENGTH] = {'\0'};

    // variables used for scanf error correction
    unsigned long long l = 0;
    int n = 0;

    // '%': indicates a field to be parsed and matched against
    // ' ': indicates any amount of whitespace. it's a special kind of field, not beginning with %
    //
    // FLAGS
    // '*': discards the following match
    // 'L': modifies the conversion from int to long long
    // <num>: indicates maximum number of character/digit occurrences for the match
    //
    // CONVERSIONS
    // '[a-z]': indicates a range of possible consecutive characters that form a string. the string will be null terminated (excluding max limit)
    // 'u': indicates an  unsigned int. The int type can be modified by a flag.
    // 'n': stores the amount of characters parsed so far into a variable. is used for checking there is no garbage left at end of string.
    int i = sscanf(command, "%*1[iec]%*1[ ] %10Lu%*1[ ] %19[a-zA-Z] %n", &l, name,  &n);

    // check for scanf() errors: 
    // 1) EOF for an matching failure or unexpected enf of input.
    // 2) errno for other errors encountered, such as string too long.
    // 3) i for correct amount of read fields, which is 2 (excluding n).
    // 4) n for unexpected data after the match is done. 
    //    Should be as long as the command in case of success, to indicate all characters have been parsed.
    // 5) INT_MAX for integer overflow (since l is guaranteed to be a positive non-overflowed and any 10 digit integer).
    if (i==EOF || errno!=0 || i!=2 || n<strlen(command) || l>INT_MAX)
        return NULL;

    age = (int) l;
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
 * 3) Always remember to deallocate data that will no longer be used, to prevent memory leaks.
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
 *
 * PROBLEMS FOUND:
 * 1) Heap allocations (mallor and realloc) were not checked for errors. 
 *    These errors can indicate an "out of memory" situation, and if not
 *    checked can result in memory leaks (such as with realloc).
 * 2) InputAt was not updated to consider possible address changes
 *    after the call to realloc().
 * 3) Memory leak for input could occur whenever the function needed to quit due to errors
 * 4) In case of fgets() error the function quits, even though it might be a legitimate situation.
 *    Such a situation could be an EOF occurring while reading the last line of a file piped to input.
 *    The last line could be something like "xEOF" which is a command that needs to be parsed correctly.
 *    In case an EOF occurs and nothing has been read at all, then there is an error.
 * 5) The inputMaxLength increase could result in an integer overflow.
 *    While this is an unlikely scenario, it must be accounted for.
 * 6) In case of byte injections (\0) into the command, there could be a buffer underflow on the last line of code.
 *   
 *
 * SOLUTIONS:
 * - while the function itself is filled with bugs, the checks and code that was added
 *   was kept to a minimum, without trying to restructure the code.
 * 1) If malloc() or realloc() fail, the function terminates with NULL.
 *    If the memory was already initialized, it is freed as well.
 * 2) If fgets() receives an EOF but a command was read, then it is not thrown away
 *    and the function returns such a string.
 *    Otherwise, an error has occurred and the input buffer is freed.
 * 3) Integer overflows cannot  be detected after occurrence, therefore must be prevented.
 *    Since an overflow may occur while increasing input buffer length (though it is quite rare),
 *    then we check that the result of such an operation doesn't overflow.
 * 4) During realloc(), a new pointer may be return to represent the new memory allocated.
 *    If this happens, then input is rightfully update.
 *    In case of errors, the original input memory may be left untouched and NULL is returned.
 *    So the update is done in a two-step fashion.
 * 5) InpuAt needs to be pointing to the the end of the input minus the increment size while also
 *    accounting for the null byte, which needs to be overwritten as well.
 * 5) During the removal of the newline character, there should be a check for \0 byte injection.
 *    Another check ought to be added in case an EOF has occurred right after the "x" command
 *    (which could be piped from a file). In such a situation, there may not be a newline at all!
 */

char* read_command(FILE* in) {
    int inputMaxLength = 0;
    char* input = NULL;
    char* inputAt = NULL;
    int incr = INPUT_INCREMENT;
    inputMaxLength = incr;
    input = (char*)malloc(sizeof(char) * incr);
    if (!input) // fix: malloc error
        return NULL;
    inputAt = input;
    do 
    {
        inputAt[incr - 1] = 'e';

        if(fgets(inputAt, incr, in) == NULL) 
        { 
            // check if EOF in a legitimate situation
            if (inputAt>input) break; // fix: EOF may occur also during correct situations
            free(input); // fix: input was leaked
            return NULL; 
        }
        if(inputAt[incr - 1] != '\0' || inputAt[incr - 2] == '\n')
            break;

        // preventive overflow check
        if (inputMaxLength > INT_MAX - INPUT_INCREMENT) // fix: possible overflow
        {
            free(input);
            return NULL;
        }
        inputMaxLength += INPUT_INCREMENT;

        // careful realloc usage
        char* tmpinput = realloc(input, sizeof(char) * inputMaxLength); // fix: realloc error, and memory leak.
        if (!tmpinput)
        {
            free(input); // fix: see memory leak on realloc()
            return NULL;
        }
        input = tmpinput; 

        // relative addressing for inputAt
        inputAt = input + inputMaxLength - INPUT_INCREMENT - 1; // fix: inputAt not relative to new input
        incr = INPUT_INCREMENT + 1;
    } while(1);

    // check for any final byte injections
    int last_pos = strlen(input) - 1 + (!input[0]? 1: 0); // error: possible byte injection if input[0]=='\0'
    if (input[last_pos]=='\n')  // error: check for exceptional "xEOF" situation, described above
        input[last_pos]='\0';
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
 *
 * ISSUES FOUND:
 * 1) Sortedcontainer_new() is not checked for success.
 * 2) Memory leaks for command can occur after each loop, or when the loop is broken.
 *
 * SOLUTIONS:
 * 1) Sortedcontainer_new() is checked for success.
 *    In case of error, sortedcontainer.c functions would have been
 *    called with a NULL parameter. These functions cannot check for
 *    invalid data as they always expect non-null pointers and valid
 *    inputs, as is the case with most C99 functions.
 * 2) Command is free'd after each loop, and also during a loop break.
 *    While in our current program the loop break free is not required
 *    (as the program free's all memory upon exit), it is still important
 *    to avoid memory leaks early on.
 */
int main(int argc, char* argv[]) 
{
    (void)argc; (void) argv;
    sortedcontainer* sc = sortedcontainer_new();
    if (!sc) // fix: initialization error
    {
        fprintf(stderr, "\nError initializing container.\n");
        return -1;
    }

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
        free(command); // fix: memory leak
    }
    sortedcontainer_delete(sc);
    fprintf(stdout, "\nBye.\n");
    return 0;
}
