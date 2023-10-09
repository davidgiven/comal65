#define _XOPEN_SOURCE 500
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <glob.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <poll.h>
#include <errno.h>
#include <libelf.h>
#include <gelf.h>
#include "globals.h"

static std::string commandline;

static void write8(uint16_t address, uint8_t b)
{
    ram[address] = b;
}

static void write16(uint16_t address, uint16_t w)
{
    write8(address + 0, w & 0xff);
    write8(address + 1, w >> 8);
}

static void write32(uint16_t address, uint32_t w)
{
    write16(address + 0, w & 0xffff);
    write16(address + 2, w >> 16);
}

static uint8_t read8(uint16_t address)
{
    return ram[address];
}

static uint16_t read16(uint16_t address)
{
    return ram[address + 0] | (ram[address + 1] << 8);
}

static uint16_t read32(uint16_t address)
{
    return read16(address + 0) | (read16(address + 2) << 16);
}

static void load_binary(std::string filename)
{
    /* Load the binary. */

    int fd = open(filename.c_str(), O_RDONLY);
    if (fd == -1)
        fatal("couldn't open program: %s", strerror(errno));
    read(fd, &ram[load_address], HIMEM - load_address);
    close(fd);

    /* If an ELF file exists, fetch symbol information from it. */

    std::string elffilename = filename + ".elf";
    if (access(elffilename.c_str(), R_OK) == 0)
    {
        /* ELF file exists; load from this. */

        elf_version(EV_NONE);
        if (elf_version(EV_CURRENT) == EV_NONE)
            fatal("bad libelf versrion");

        int fd = open(elffilename.c_str(), O_RDONLY);
        if (fd == -1)
            fatal("couldn't open program: %s", strerror(errno));
        Elf* elf = elf_begin(fd, ELF_C_READ, nullptr);
        if ((elf_kind(elf) != ELF_K_ELF))
            fatal("not an ELF file");

        GElf_Phdr phdr;
        if ((gelf_getphdr(elf, 0, &phdr) != &phdr) || (phdr.p_type != PT_LOAD))
            fatal("could not fetch main data block from ELF file: %s",
                elf_errmsg(-1));

        uint16_t loadAddress = phdr.p_vaddr;

        Elf_Scn* scn = nullptr;
        GElf_Shdr shdr;
        for (;;)
        {
            scn = elf_nextscn(elf, scn);
            if (!scn)
                break;

            gelf_getshdr(scn, &shdr);
            if (shdr.sh_type == SHT_SYMTAB)
            {
                Elf_Data* data = elf_getdata(scn, NULL);
                int count = shdr.sh_size / shdr.sh_entsize;

                /* print the symbol names */
                for (int i = 0; i < count; ++i)
                {
                    GElf_Sym sym;
                    gelf_getsym(data, i, &sym);

                    std::string name =
                        elf_strptr(elf, shdr.sh_link, sym.st_name);
                    uint16_t address = sym.st_value;

                    symbolsByName[name] = address;
                    symbolsByAddress[address] = name;
                }
            }
        }

        elf_end(elf);
        close(fd);
    }
}

void mos_boot(void)
{
    M6502_reset(cpu);

    if (user_command_line[0])
    {
        static bool terminate_next_time = false;
        if (terminate_next_time)
            exit(1);
        terminate_next_time = true;

        /* Push the return address onto the stack. */
        ram[0x01fe] = (EXIT_ADDRESS - 1) & 0xff;
        ram[0x01ff] = (EXIT_ADDRESS - 1) >> 8;
        cpu->registers->s = 0xfd;

        load_binary(user_command_line[0]);

        /* Generate the command line. */

        uint8_t* p = &ram[0x0300];

        for (int word = 1; user_command_line[word]; word++)
        {
            if (word > 1)
                commandline += ' ';

            const char* pin = user_command_line[word];
            while (*pin)
            {
                if (commandline.size() > 253)
                    fatal("user command line too long");
                commandline += *pin++;
            }
        }

        cpu->registers->pc = exec_address;
    }
    else
        fatal("no user command provided");

    ram[0x01fe] = (EXIT_ADDRESS - 1) & 0xff; // lo
    ram[0x01ff] = (EXIT_ADDRESS - 1) >> 8;   // hi
    cpu->registers->s = 0xfd;
}

void osargs(void)
{
    uint8_t fd = cpu->registers->y;
    uint16_t block = (uint16_t)cpu->registers->x;

    if (fd == 0)
    {
        switch (cpu->registers->a)
        {
            case 0x01: /* get address of command line */
                write32(block, 0x300);
                break;

            default:
                printf("unknown OSARGS y=0 0x%02x\n", cpu->registers->a);
                exit(1);
        }
    }
    else
    {
        switch (cpu->registers->a)
        {
            default:
                printf("unknown OSARGS y!=0 0x%02x\n", cpu->registers->a);
                exit(1);
        }
    }
}

void osbget(void)
{
    printf("unknown OSBGET\n");
    exit(1);
}

void osfile(void)
{
    uint16_t block =
        (uint16_t)cpu->registers->x | ((uint16_t)cpu->registers->y << 8);

    switch (cpu->registers->a)
    {
        default:
            printf("unknown OSFILE 0x%02x\n", cpu->registers->a);
            exit(1);
    }
}

void osbyte(void)
{
    switch (cpu->registers->a)
    {
        case 0x82: /* read high-order address */
            cpu->registers->x = cpu->registers->y = 0;
            break;

        case 0x83: /* read HWM */
            cpu->registers->x = 0;
            cpu->registers->y = 0x08;
            break;

        case 0x84: /* read HIMEM (on I/O processor) */
            cpu->registers->x = 0;
            cpu->registers->y = 0x80;
            break;

        default:
            printf("unknown OSBYTE: a=0x%02x x=0x%02x y=0x%02x\n",
                cpu->registers->a,
                cpu->registers->x,
                cpu->registers->y);
            exit(1);
    }
}

void osword(void)
{
    uint16_t block =
        (uint16_t)cpu->registers->x | ((uint16_t)cpu->registers->y << 8);

    switch (cpu->registers->a)
    {
        case 0x00: /* read line */
        {
            uint16_t bufaddr = read16(block + 0);
            uint8_t max = ram[block + 2];
            uint8_t count = 0;

            for (;;)
            {
                uint8_t c;
                read(0, &c, 1);
                if (c == 10)
                    c = 13;

                if (count < max)
                {
                    ram[bufaddr + count] = c;
                    count++;
                }
                if (c == 13)
                    break;
            }
            cpu->registers->p = 0;
            cpu->registers->y = count - 1;
            break;
        }

        case 0x05: /* read I/O RAM */
        {
            uint32_t ioaddr = read32(block + 0);
            switch (ioaddr)
            {
                case 0xf2:
                    ram[block + 4] = 0x00;
                    break;
                case 0xf3:
                    ram[block + 4] = 0x02;
                    break;
                case 0x02ff:
                    ram[block + 4] = '\r';
                    break; /* command line */
                default:
                    if ((ioaddr >= 0x300) && (ioaddr <= 0x3ff))
                    {
                        int offset = ioaddr - 0x300;
                        if (offset < commandline.size())
                            ram[block + 4] = commandline[offset];
                        else
                            ram[block + 4] = 0x0d;
                    }
                    else
                        ram[block + 4] = 0x00;
                    break;
            }
            break;
        }

        default:
            printf("unknown OSWORD: a=0x%02x x=0x%02x y=0x%02x\n",
                cpu->registers->a,
                cpu->registers->x,
                cpu->registers->y);
            exit(1);
    }
}

void oswrch(void)
{
    char c = cpu->registers->a;
    write(1, &c, 1);
}

void osnewl(void)
{
    static const char c = '\n';
    write(1, &c, 1);
}

void osasci(void)
{
    char c = cpu->registers->a;
    if (c == 13)
        osnewl();
    else
        write(1, &c, 1);
}
