#ifndef GLOBALS_H
#define GLOBALS_H

#include <stdbool.h>
#include <string>
#include <map>
extern "C"
{
#include "third_party/lib6502/lib6502.h"
}

#define EXIT_ADDRESS 0xfffe
#define HIMEM 0xf800

extern M6502* cpu;
extern uint8_t ram[0x10000];
extern uint16_t himem;
extern std::map<std::string, uint16_t> symbolsByName;
extern std::map<uint16_t, std::string> symbolsByAddress;
extern uint16_t load_address;
extern uint16_t exec_address;

extern void emulator_init(void);
extern void emulator_run(void);
extern void showregs(void);

extern void mos_boot(void);

extern void osbget();
extern void osbput();
extern void osargs();
extern void osfile();
extern void osword();
extern void osbyte();
extern void oswrch();
extern void osnewl();
extern void osasci();

extern void fatal(const char* message, ...);

extern bool flag_enter_debugger;
extern char* const* user_command_line;

#endif
