#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <getopt.h>
#include <ctype.h>
#include "globals.h"

bool flag_enter_debugger = false;
char* const* user_command_line = NULL;
uint16_t load_address = 0x0400;
uint16_t exec_address = 0x0400;

void fatal(const char* message, ...)
{
    va_list ap;
    va_start(ap, message);
    fprintf(stderr, "fatal: ");
    vfprintf(stderr, message, ap);
    fprintf(stderr, "\n");
    exit(1);
}

static void syntax(void)
{
    printf("cpm [<flags>] [command] [args]:\n");
    printf("  -h             this help\n");
    printf("  -d             enter debugger on startup\n");
    printf("  -l 0x1234      load address (by default, 0x0400)\n");
    printf("  -e 0x1234      execution address (by default, 0x0400)\n");
    printf(
        "If command is specified, a Unix file of that name will be loaded "
        "and\n");
    printf(
        "injected directly into memory (it's not loaded through the CCP).\n");
    printf("The first two arguments are mapped to the standard FCBs.\n");
    exit(1);
}

static void parse_options(int argc, char* const* argv)
{
    for (;;)
    {
        switch (getopt(argc, argv, "hdp:m:"))
        {
            case -1:
                goto end_of_flags;

            case 'd':
                flag_enter_debugger = true;
                break;

            case 'l':
				load_address = strtoul(optarg, NULL, 0);
                break;

            case 'e':
				exec_address = strtoul(optarg, NULL, 0);
                break;

            default:
                syntax();
        }
    }

end_of_flags:
    user_command_line = &argv[optind];
}

int main(int argc, char* const* argv)
{
    parse_options(argc, argv);

    emulator_init();
    mos_boot();

    for (;;)
        emulator_run();

    return 0;
}
