#ifdef TARGET_DEFS_ONLY

#define LDOUBLE_SIZE 12 // not actually supported
#define LDOUBLE_ALIGN 4
#define MAX_ALIGN 16

#define NB_REGS 7

#define RC_INT 0x0001
#define RC_FLOAT 0x0002
#define RC_A 0x0004
#define RC_B 0x0008
#define RC_C 0x0010
#define RC_X 0x0020
#define RC_Y 0x0040
#define RC_Z 0x0080
#define RC_I 0x0100
#define RC_J 0x0200

#define RC_IRET RC_A /* function return: integer register */
#define RC_LRET RC_A /* function return: second integer register */
#define RC_FRET RC_A /* function return: float register */

enum {
    TREG_A = 0,
    TREG_B,
    TREG_C,
    TREG_X,
    TREG_Y,
    TREG_Z,
    TREG_I
};


/*
    Undefine this to avoid using the stack so muck.

    This will break the C ABI tho.
*/
#undef USE_REGS_FOR_LOCALS


#ifndef USE_REGS_FOR_LOCALS
ST_DATA const int reg_classes[NB_REGS] = {
    RC_INT | RC_A,
    RC_INT | RC_B,
    RC_INT | RC_C,
    RC_INT | RC_X,
    RC_INT | RC_Y,
    RC_INT | RC_Z,
    RC_INT | RC_I,
};

#else

ST_DATA const int reg_classes[NB_REGS] = {
    RC_INT | RC_A,
    RC_B,
    RC_C,
    RC_X,
    RC_Y,
    RC_INT | RC_Z,
    RC_INT | RC_I,
};

#define NUM_LOCAL_REGS 4
#endif

#define REG_IRET TREG_A /* single word int return register */
#define REG_LRET TREG_A /* second word return register (for long long) */
#define REG_FRET TREG_A /* float return register */

#define R_DATA_32 1 // whatever
#define R_DATA_PTR 1 // whatever
#define R_JMP_SLOT 2 // whatever
#define R_COPY 3 // whatever

#define ELF_PAGE_SIZE 0x100 // whatever
#define ELF_START_ADDR 0x0

#define PTR_SIZE 2

#define EM_TCC_TARGET EM_DCPU16

#define LOCAL_LABEL "__local_%d"

#else

#include "tcc.h"
#include "stdbool.h"
#include "limits.h"


/*****************************************************************************
 *
 * Helper definitions.
 *
 */


#include "helper-dump.h"


/*****************************************************************************
 *
 * Helper definitions.
 *
 */

#ifdef FUNC_STRUCT_PARAM_AS_PTR
#error "Structs passed as pointers not supported"
#endif

#include <stdarg.h>

#define Log(...) do{ \
    emit_printf(" ; "); emit_printf(__VA_ARGS__); emit_printf("\n"); \
    printf(__VA_ARGS__); puts(""); \
} while (0)

#define UNSUPPORTED(...) do { \
    printf("@ %s %s: %d\n", __FILE__,  __func__, __LINE__); \
    tcc_error(__VA_ARGS__); \
} while (0)


/*****************************************************************************
 *
 * Internal emit functions.
 *
 */

/* Instructions */
typedef enum {
    // Basic instructions
    NONBASIC, SET, ADD, SUB, MUL, DIV, MOD, SHL,
    SHR, AND, BOR, XOR, LFE, LFN, LFG, LFB,

    // Extended instructions
    EXT_RESERVED, EXT_JSR,

    // Dtools Extended instructions
    EXT_SYS
} DIns;

/* Value encoding */
typedef enum {
    DV_A, DV_B, DV_C, DV_X, DV_Y, DV_Z, DV_I, DV_J,
    DV_REFBASE = 0x08, DV_REFTOP = 0x0f,
    DV_REF_REG_NEXTWORD_BASE = 0x10,  DV_REF_REG_NEXTWORD_TOP= 0x17,
    DV_POP, DV_PEEK, DV_PUSH,
    DV_SP, DV_PC,
    DV_O,
    DV_REF_NEXTWORD, DV_NEXTWORD,
    DV_LITERAL_BASE, DV_LITERAL_TOP = 0x3f,
    DV_LITERAL_NUM = DV_LITERAL_TOP - DV_LITERAL_BASE + 1,
} DVals;

static char* dins_names[] = {
    "NONBASIC", "SET", "ADD", "SUB", "MUL", "DIV", "MOD", "SHL", "SHR",
    "AND", "BOR", "XOR", "IFE", "IFN", "IFG", "IFB", "RESERVED_EXTENDED", "JSR", "SYS"
};

static char* dval_names[] = {
    "A", "B", "C", "X", "Y", "Z", "I", "J",
    "[A]", "[B]", "[C]", "[X]", "[Y]", "[Z]", "[I]", "[J]",
    "[NW+A]", "[NW+B]", "[NW+C]", "[NW+X]", "[NW+Y]", "[NW+Z]", "[NW+I]", "[NW+J]",
    "[SP++]", "[SP]", "[--SP]",
    "SP", "PC",
    "O",
    "[NW]", "NW",
    "0x00", "0x01", "0x02", "0x03", "0x04", "0x05", "0x06", "0x07",
    "0x08", "0x09", "0x0A", "0x0B", "0x0C", "0x0D", "0x0E", "0x0F",
    "0x10", "0x11", "0x12", "0x13", "0x14", "0x15", "0x16", "0x17",
    "0x18", "0x19", "0x1A", "0x1B", "0x1C", "0x1D", "0x1E", "0x1F"
};

#define DINS_NUM (EXT_SYS + 1)
#define DIS_NUM_BASIC (IFB + 1)
#define DINS_EXT_BASE (EXT_RESERVED)

#define HAS_NEXTWORD(v) \
    ((v > DV_REF_REG_NEXTWORD_BASE && v <= DV_REF_REG_NEXTWORD_TOP) \
     || v == DV_REF_NEXTWORD || v == DV_NEXTWORD)

/*
   This function writes emitted opcode into the data section.

   XXX Could be made a lot faster.
 */

void i_emit(uint16_t op)
{
    size_t newSize = ind + 2;

    if (newSize > cur_text_section->data_allocated)
        section_realloc(cur_text_section, newSize);

    cur_text_section->data[ind] = op & 0xff;
    cur_text_section->data[ind + 1] = (op >> 8) & 0xff;
    ind = newSize;
}

void i_emit8(uint8_t op)
{
    size_t newSize = ind + 1;

    if (newSize > cur_text_section->data_allocated) {
        section_realloc(cur_text_section, newSize);
    }

    cur_text_section->data[ind] = op;
    ind = newSize;
}

void i_emit_ins_binary(DIns opcode, DVals ta, uint16_t nwa, DVals tb, uint16_t nwb)
{
    i_emit((opcode & 0xf) | ((ta & 0x3f) << 4) | ((tb & 0x3f) << 10));

    if (HAS_NEXTWORD(ta)) i_emit(nwa);
    if (HAS_NEXTWORD(tb)) i_emit(nwb);
}

char* i_str_replace(char* target, const char* str, const char* what, const char* with)
{
    const char* ss = strstr(str, what);

    if (!ss) {
        strcpy(target, str);
    } else {
        int at = (intptr_t)ss - (intptr_t)str;

        strncpy(target, str, at);
        strcpy(target + at, with);
        strcpy(target + at + strlen(with), str + at + strlen(what));
    }

    return target;
}

void i_emit_ins_ascii(DIns opcode, DVals ta, uint16_t nwa, DVals tb, uint16_t nwb)
{
    char buffer[0x100];
    memset(buffer, 0, sizeof(buffer));

    int num_vals = 2, i = 0;
    DVals val[] = {ta, tb};
    uint16_t nw[] = {nwa, nwb};

    int len = sprintf(buffer, "\t%s", dins_names[opcode]);

    if (opcode == NONBASIC) {
        num_vals = 1;
        opcode = val[0] + DINS_EXT_BASE;
        val[0] = val[1];
        nw[0] = nw[1];
    }

    for (i = 0; i < num_vals; i++) {
        char numStr[64] = {0}, str[64] = {0};

        if (HAS_NEXTWORD(val[i]))
            sprintf(numStr, "0x%x", nw[i]);

        len += sprintf(buffer + len, "%s", i_str_replace(str, dval_names[val[i]], "NW", numStr));
        if (i == 0 && num_vals == 2)
            len += sprintf(buffer + len, ", ");
    }

    buffer[len++] = '\n';

    for (i = 0; i < len; i++) {
        i_emit8(buffer[i]);
    }
}


/*****************************************************************************
 *
 * Emit functions used by the code gen functions.
 *
 */


/*
    When emitting assambler calls to this function
    gets turned comments in the source file.
*/

void emit_printf(const char* format, ...)
{
    va_list fmtargs;
    char buffer[512];
    int len, i;

    if (!tcc_state->gen_asm)
        return;

    memset(buffer, '\0', 512);

    va_start(fmtargs, format);
    len = vsprintf(buffer, format, fmtargs);
    va_end(fmtargs);

    for (i = 0; i < len; i++)
        i_emit8(buffer[i]);
}


/*
    Emits an instruction with 1, 2 or 3 words
    ta, tb - type or value of first and second operand
    nwa, nwb - value of any used nextword words

    1 word instruction - Add a literal 3 to register A, nwa and nwb unused
    emit_ins(ADD, DV_A, 0, DV_LITERALBASE + 3, 0);

    3 word instruction -
    XOR what's found at memory location [0x123+b] with [0x100] and save to
    [0x123+b]

    emit_ins(XOR, DV_REF_REG_NEXTWORD_BASE + DV_B, 0x123,
    DV_REF_NEXTWORD, 0x100);
*/

void (*emit_ins)(DIns opcode, DVals ta, uint16_t nwa, DVals tb, uint16_t nwb);


/*
    Emits codes that read from the frame pointer address,
    offseted by the given offset, to any GP.

    Where the frame pointer is J.

    // SET reg, [J+offset]
*/

void emit_read_stack(int offset, int reg)
{
    if (offset < SHRT_MIN || offset > SHRT_MAX)
        tcc_error("ICE: stack read access out of bounds!");

    if (offset)
        emit_ins(SET, reg, 0, DV_REF_REG_NEXTWORD_BASE + DV_J, offset);
    else
        emit_ins(SET, reg, 0, DV_REFBASE + DV_J, 0);
}


/*
    Emits codes that writes to the frame pointer address,
    offsetted by the given offset, to any GP.

    Where the frame pointer is J.

    // SET [J+offset], reg
*/

void emit_write_stack(int offset, int reg)
{
    if (offset < SHRT_MIN || offset > SHRT_MAX)
        tcc_error("ICE: stack write access out of bounds!");

    if (offset)
        emit_ins(SET, DV_REF_REG_NEXTWORD_BASE + DV_J, offset, reg, 0);
    else
        emit_ins(SET, DV_REFBASE + DV_J, 0, reg, 0);
}


/*
    Helper function to emit simple arithmatical
    opcodes on the form "a = a 'op' b". Handles
    constants either as a literal or a nextword.

    op - the operation to be used
    a - always a register offset
    b - always a register offset
    c - c a constant value passed as a other argument
        to safely work around C int conversion rules.
    is_const - use c instead of b, stored as a literal
        or a nextword.

    1 word instruction to add a and b together.
    emit_simple_math(ADD, 0, 1, 0, false);
    becomes
    emit_ins(ADD, DV_A, 0, DV_B, 0);


    1 word instruction to shift a by 5.
    emit_simple_math(SHL, 0, 0, 5, true);
    becomes
    emit_ins(SHL, DV_A, 0, DV_LITERAL_BASE + 5, 0);


    2 word instruction to add 1337 to a.
    emit_simple_math(ADD, 0, 0, 1337, true);
    becomes
    emit_ins(ADD, DV_A, 0, DV_NEXTWORD, 1337);
*/

void emit_simple_math(DIns op, int a, int b, uint16_t c, bool b_is_const)
{
    if (b_is_const) {
        if (c < DV_LITERAL_NUM)
            emit_ins(op, DV_A + a, 0, DV_LITERAL_BASE + c, 0);
        else
            emit_ins(op, DV_A + a, 0, DV_NEXTWORD, c);
    } else {
        emit_ins(op, DV_A + a, 0, DV_A + b, 0);
    }
}


/*
    Emit an extended instruction
*/

void emit_ext_ins(DIns ins, DVals ta, uint16_t nwa)
{
    emit_ins(NONBASIC, ins - DINS_EXT_BASE , 0, ta, nwa);
}


/*
    Emits a JSR function thats is patched if sym != NULL.
*/

void emit_jsr(Sym *sym, uint16_t c)
{
    emit_ext_ins(EXT_JSR, DV_NEXTWORD, c/2);

    if (sym != NULL)
        greloc(cur_text_section, sym, ind - 2, R_DCPU_16_ADDR);
}


/*
    Push a register or a constant value to the stack,
    handles symbols lookup as well, but only for constant.

    r - Either the register number, if VT_SYM is set sym is used.
    sym - Used if VT_SYM in r only supported if is_const is true.
    c - Constant value used if is_const is set.
    is_const - Emit a constant value instead of a register read.

    If sym is used c is always put into a nextword, if not
    this function try to be smart and use literals.
*/

void emit_push_to_stack(int r, Sym *sym, uint16_t c, bool is_const)
{
    if (!is_const && (r & VT_SYM))
        UNSUPPORTED("Can not push a register with symbol patch");

    if (!is_const) {
        emit_ins(SET, DV_PUSH, 0, DV_A + r, 0);
    } else if (r & VT_SYM) {
        emit_ins(SET, DV_PUSH, 0, DV_NEXTWORD, c);
        greloc(cur_text_section, sym, ind - 2, R_DCPU_16_ADDR);
    } else {
        emit_simple_math(SET, DV_PUSH, c, c, is_const);
    }
}


/*
   Emits a SET [nextword], tb.

   c is placed in nextword and is patched with the sym reloc.
   Handles if tb has nextword correctly as well.
*/

void emit_write_to_symbol(Sym *sym, uint16_t c, DVals tb, uint16_t nwb)
{
    emit_ins(SET, DV_REF_NEXTWORD, c, tb, nwb);
    greloc(cur_text_section, sym, HAS_NEXTWORD(tb) ? ind - 4 : ind - 2, R_DCPU_16_ADDR);
}


/*
   Emits a SET [ta (+ nwa)], nextword.
   Placing the value of nextword (the address to the symbol) into ta.

   Handles if ta has nextword correctly as well.
   c is placed in nextword and is patched with the sym reloc.
*/

void emit_read_symbol(Sym *sym, uint16_t c, DVals ta, uint16_t nwa)
{
    emit_ins(SET, ta, nwa, DV_NEXTWORD, c);
    greloc(cur_text_section, sym, ind - 2, R_DCPU_16_ADDR);
}


/*
    Emit a JSR nextword opcode.
    Nextword is set to prev_refering_loc_to_same_label.

    The smybol relocaction isn't issued here but is instead
    delayed. Some callers like gjmp_addr will issue it direct
    others will wait until that location of the jmp is known.

    prev_refering_loc_to_same_label - These are used as a linked
        list to old locations that point to the same list.
        gsym_addr will walk this list and emit grelocs to it.
*/

int emit_jump_to_symbol(int prev_refering_loc_to_same_label)
{
    emit_ins(SET, DV_PC, 0, DV_NEXTWORD, prev_refering_loc_to_same_label);

    return ind - 2;
}


/*****************************************************************************
 *
 * Called by tcc.
 *
 */


/*
    Since we don't know how many local variables are going to
    be used in a function we need reserve some space at the
    start of the function and then patch that location.
 */

int global_function_prolog_sp_location;


/*
    This function handles where VT_LOCAL is located.

    For all others they are relative to the frame pointer
    and we have to take care where exactly they are.

    We might even optimize some local vars to be in registers.
*/

int global_get_vt_local_location(int addr, bool *is_register, bool is_reading_from)
{
    addr /= 2;

#ifndef USE_REGS_FOR_LOCALS
    *is_register = false;

    return addr;
#else
    if (-4 <= addr && addr <= -1) {
        *is_register = true;
        printf("is nice\n");
        return DV_A - addr;
    } else {
        *is_register = false;
        return addr;
    }
#endif

}


/*
    Init this module.
*/

void gen_init()
{
    emit_ins = tcc_state->gen_asm ? i_emit_ins_ascii : i_emit_ins_binary;
    global_function_prolog_sp_location = 0;
}


/*
    load 'r' from value 'sv'
*/

ST_FUNC void load(int r, SValue *sv)
{
    Log("%s: %i %p (%i, %i, %i)", __func__, r, sv, sv->r, sv->type.t, sv->c.ul);

    bool is_register = false;
    bool pure_indirect = false; // SET r, [sv]
    int regf = sv->r;           // flags & register
    int type_def = sv->type.t;  // type definition
    int addr = sv->c.ul;        // address
    Sym* sym = sv->sym;         // symbol information
    
    int align, size = type_size(&vtop[0].type, &align);

    int v = regf & VT_VALMASK;

    if ((regf & VT_SYM) &&
        ((regf & VT_LVAL) ||
        v != VT_CONST))
        UNSUPPORTED("can't load symbol lookups");

    if (regf & VT_LVAL) {
        if (v == VT_LLOCAL) {
            // Definitely not sure if this is correct. Stolen from x86 and C67

            SValue v1;

	    v1.type.t = VT_INT;
	    v1.r = VT_LOCAL | VT_LVAL;
	    v1.c.ul = addr;

	    load(r, &v1);

	    regf = r;
            v = regf & VT_VALMASK;
        }

        if (v == VT_LLOCAL) {
            UNSUPPORTED("double VT_LLOCAL HUH?");
        } else if (v == VT_CONST) {
            UNSUPPORTED("loading from VT_LVAL not supported (v == VT_CONST)");
        } else if (v == VT_CMP) {
            UNSUPPORTED("loading from VT_LVAL not supported (v == VT_CMP)");
        } else if (v == VT_JMP) {
            UNSUPPORTED("loading from VT_LVAL not supported (v == VT_JMP)");
        } else if (v == VT_JMPI) {
            UNSUPPORTED("loading from VT_LVAL not supported (v == VT_JMPI)");
        } else if (v < VT_CONST) {
            // SET r, [v]
            pure_indirect = true;
        }

        int val_type = type_def & VT_TYPE;

        if (!(val_type == VT_INT || val_type == (VT_INT | VT_UNSIGNED) ||
              val_type == VT_SHORT || val_type == (VT_SHORT | VT_UNSIGNED) ||
              val_type == VT_BYTE || val_type == (VT_BYTE | VT_UNSIGNED) ||
              val_type == VT_PTR)) {
            UNSUPPORTED("unsupported format to load from (%u)", val_type);
        }

        if (pure_indirect) {

            // SET r, [v]
            emit_ins(SET, DV_A+r, 0, DV_REFBASE + v, 0);

        } else {

            // SET r, [SP + addr]

            addr = global_get_vt_local_location(addr, &is_register, true);

            if (!is_register) {
                emit_read_stack(addr, DV_A + r);
            } else if (r != addr) {
                emit_ins(SET, DV_A + r, 0, DV_A + addr, 0);
            }
        }

    } else {
        if (v == VT_CONST) {

            if (regf & VT_SYM) {

                // SET r, sym
                emit_read_symbol(sym, addr, r, 0);

            } else {

                // SET r, value

                // Not really a address but thats where the data is.
                emit_simple_math(SET, r, 0, addr, true);
            }

        } else if (v == VT_LOCAL) {
            UNSUPPORTED("loading VT_LOCAL to reg not supported");
        } else if (v == VT_CMP) {
            UNSUPPORTED("loading VT_CMP to reg not supported");
        } else if (v == VT_JMP) {
            UNSUPPORTED("loading VT_JMP to reg not supported");
        } else if (v == VT_JMPI) {
            UNSUPPORTED("loading VT_JMPI to reg not supported");
        } else if (v < VT_CONST && v != r) {

            // SET r, v
            emit_ins(SET, r, 0, v, 0);

        } else {
            UNSUPPORTED("uncought case in load");
        }
    }
}


/*
    Store register 'r' in lvalue 'sv'
*/

ST_FUNC void store(int r, SValue *sv)
{
    Log("%s: %u %p", __func__, r, sv);

    int regf = sv->r;                  // flags & register
    int addr = sv->c.ul;               // address
    int type_def = sv->type.t;         // type definition
    int val_type = type_def & VT_TYPE; // with out storage and modifier
    int v = regf & VT_VALMASK;         // value information
    bool is_register = false;
    Sym* sym = sv->sym;


    if (!(regf & VT_LVAL)) {
        UNSUPPORTED("can only store to VT_LVAL");
    }

    if (!(val_type == VT_INT || val_type == (VT_INT | VT_UNSIGNED) ||
          val_type == VT_SHORT || val_type == (VT_SHORT | VT_UNSIGNED) ||
          val_type == VT_BYTE || val_type == (VT_BYTE | VT_UNSIGNED) ||
          val_type == VT_PTR)) {
        if ((type_def & VT_BTYPE) == VT_PTR && (val_type & VT_CONSTANT))
            tcc_error("can't write to a constant pointer");
        else
            UNSUPPORTED("unsupported format to store to (%u)", val_type);
    }

    // Paraniod checking.
    if ((regf & VT_SYM) && v != VT_CONST) {
        UNSUPPORTED("can only handle symbol lookups with const");
    }

    if (v == VT_LOCAL) {

        // SET [SP + addr], r

        addr = global_get_vt_local_location(addr, &is_register, false);

        if (!is_register) {
            emit_write_stack(addr, DV_A + r);
        } else if (addr != r) {
            emit_ins(SET, DV_A + addr, 0, DV_A + r, 0);
        }

    } else if (v == VT_CONST) {

        // SET [addr], r

        if (regf & VT_SYM) {

            // The offset is generated by tcc and is good.
            emit_write_to_symbol(sym, addr, DV_A + r, 0);

        } else {

            // The address is comming directly from code no need to devide by 2.

            if (addr < DV_LITERAL_NUM)
                emit_ins(SET, DV_LITERAL_BASE + addr, 0, DV_A + r, 0);
            else
                emit_ins(SET, DV_REF_NEXTWORD, addr, DV_A + r, 0);
        }

    } else if (v < VT_CONST) {

        // SET [v], r
        emit_ins(SET, DV_REFBASE + v, 0, DV_A + r, 0);

    } else {
        UNSUPPORTED("unsupported value store type (%u)", v);
    }
}


/*
    Do integer arithmatics. The top value on the
    stack is b and the second is a.

    We try to inline constant additions into the opcode.

    We also have to take extra care with maths on pointers
    as pointer derefs as the compiler expects to be able
    to address byte wise.
*/

ST_FUNC void gen_opi(int op)
{
    Log(__func__);

    int align, size = type_size(&vtop[0].type, &align);
    int r = vtop[-1].r;
    int r_basic_type = vtop[-1].type.t & VT_BTYPE;
    int fr = vtop[0].r;
    int fc = vtop[0].c.ul;
    bool top_is_const = false;
    bool top_is_register = false;


    // get the actual values
    if ((fr & VT_VALMASK) == VT_CONST) {

        // truncate and make it pass the constant around in fr.
        fr = (short)fc;
        top_is_const = true;

    } else if ((fr & VT_LVAL) &&
               (fr & VT_VALMASK) == VT_LOCAL) {

        // vtop might be a argument to this function that has been
        // passed as register according to the ABI.
        fr = global_get_vt_local_location(fc, &top_is_register, false);
    }


    if (top_is_register || top_is_const) {

        // don't read from top, already handled
        vtop--;
        r = gv(RC_INT);

    } else {

        // have to load both operands to registers
        gv2(RC_INT, RC_INT);

        r = vtop[-1].r;
        fr = vtop[0].r;

        // our return should be on the top of the stack
        vtop--;
    }

    switch(op) {
    case TOK_ADDC2: // with carry use.
        emit_ins(ADD, DV_A + r, 0, DV_O, 0);
    case '+':
    case TOK_ADDC1: // with carry generation.
        emit_simple_math(ADD, r, fr, fr, top_is_const);
        break;

    case TOK_SUBC2: // with carry use.
        emit_ins(SUB, DV_A + r, 0, DV_O, 0);
    case '-':
    case TOK_SUBC1: // with carry generation.
        emit_simple_math(SUB, r, fr, fr, top_is_const);
        break;
    case '/':
        emit_simple_math(DIV, r, fr, fr, top_is_const);
        break;
    case '*':
        emit_simple_math(MUL, r, fr, fr, top_is_const);
        break;

    case TOK_NE:
        emit_simple_math(XOR, r, fr, fr, top_is_const);
	emit_simple_math(LFN, r, 0, 0, true);
	emit_simple_math(SET, r, 1, 0, true);
    case TOK_GT:
        emit_simple_math(XOR, DV_O, DV_O, 0, false);
        emit_simple_math(LFG, r, fr, fr, top_is_const);
        emit_simple_math(SET, DV_O, 0, 1, true);
        emit_simple_math(SET, r, DV_O, 0, false);
        break;
    case TOK_EQ:
        emit_simple_math(XOR, DV_O, DV_O, 0, false);
        emit_simple_math(LFE, r, fr, fr, top_is_const);
        emit_simple_math(SET, DV_O, 0, 1, true);
        emit_simple_math(SET, r, DV_O, 0, false);
        break;

    default:
        UNSUPPORTED("unsupported integer operation (%x)", op);
    }
}

ST_FUNC void gfunc_prolog(CType *func_type)
{
    Log(__func__);

    Sym* sym = func_type->ref;
    CType *type = NULL;
    int i = 0;
    int align = 0, size = 0;
    int addr = 0;
    int func_vc = 0;
    int param_index = 0;
    int nb_reg_args = 0;

    // A global var, that tells where the most
    // rescent local variable has been placed.
    loc = 0;

    // XXX What does func_vt do?
    func_vt = sym->type;


    /* if the function returns a structure, then add an
       implicit pointer parameter */
    if ((func_vt.t & VT_BTYPE) == VT_STRUCT) {
        UNSUPPORTED("does not support struct returns");
    }


    /* define parameters */
    while ((sym = sym->next) != NULL) {
        type = &sym->type;
        size = type_size(type, &align);

        if (size != 2) { // XXX Or one
            UNSUPPORTED("typed passed to function does not have size 2 (%i)", size);
        }

        // Pass some args via registers.
        if (nb_reg_args < 3) {
            nb_reg_args++;
            loc -= 2;
            addr = loc;
        } else {
            // Return addres and J is pushed that leaves 1.
            addr = (param_index - 1) * 2;
        }

        sym_push(sym->v & ~SYM_FIELD, type,
                VT_LOCAL | lvalue_type(type->t), addr);

        param_index++;
    }

    // We need to ensure that J is preserved for the caller.
    emit_ins(SET, DV_PUSH, 0, DV_J, 0);

    // Use J as a stack pointer.
    emit_ins(SET, DV_J, 0, DV_SP, 0);

#ifndef USE_REGS_FOR_LOCALS
    // Push the argument registers passed to the stack.
    for (i = 0; i < nb_reg_args; i++) {
         emit_write_stack(-1-i, DV_A + i);
    }
#endif

    // Save space for the "SUB SP, num_locals" prolog.
    global_function_prolog_sp_location = ind;
    ind += 4;
}


/*
    The end of the function.

    Need to patch the stack pointer to save room for
    local variables.
*/

ST_FUNC void gfunc_epilog(void)
{
    int saved_ind = 0;

    int v = -(loc / 2);

    Log(__func__);

    // Restore saved space on the stack for the local vars.
    emit_simple_math(ADD, DV_SP, 0, v, true);

    // Restore J
    emit_ins(SET, DV_J, 0, DV_POP, 0);

    // Return to the caller by poping the return address and setting PC.
    emit_ins(SET, DV_PC, 0, DV_POP, 0);


    /*
     * Patch the prolog_sp SUB with the number of local variables.
     *
     * This jumps back to the top of the function and inserts
     *
     * SUB, DV_SP, num_local_vars.
     */

    saved_ind = ind;
    ind = global_function_prolog_sp_location;


    // Move the real stack for local variables.
    emit_ins(SUB, DV_SP, 0, DV_NEXTWORD, v);

    ind = saved_ind;
}


/*
    Generate function call. The function address is pushed first, then
    all the parameters in call order. This functions pops all the
    parameters and the function address.
*/

ST_FUNC void gfunc_call(int nb_args)
{
    int arg_type = 0;
    int arg_register_type = 0;
    int i = 0;
    int r = 0;
    int value = 0;
    int tmp = 0;
    int nb_pushed_args = 0;
    bool arg_is_const = false;

    Sym *sym = NULL;

    Log("%s: num args %u", __func__, nb_args);

    save_regs(0);

#ifdef USE_REGS_FOR_LOCALS
    // Save regs used for locals.
    int regs_to_save = -(loc / 2);
    for (i = 1; i <= regs_to_save && i <= NUM_LOCAL_REGS; i++) {
         emit_write_stack(-i, DV_A + i);
    }
#endif

    for (i = 0; i < nb_args; i++) {
        arg_register_type = vtop[0].r & VT_VALMASK;
        arg_type = vtop[0].type.t & VT_BTYPE;
        r = vtop[0].r;

        switch (arg_type) {
        case VT_INT:
        case VT_INT | VT_UNSIGNED:
        case VT_SHORT:
        case VT_SHORT | VT_UNSIGNED:
        case VT_PTR:

            // First 3 arguments are passed by register.
            tmp = nb_args - i - 1;
            if (tmp < 3) {
                r = gv(RC_A << tmp);
                break;
            }

            if (arg_register_type == VT_CONST) {
                value = vtop[0].c.ul;
                arg_is_const = true;
            } else if (r & VT_SYM) {
                UNSUPPORTED("cannot patch a non constant value");
            } else {
                arg_is_const = false;
                value = 0;
                // move the value into a register so we can push it.
                r = gv(RC_INT);
            }

            emit_push_to_stack(r, vtop[0].sym, value, arg_is_const);
            nb_pushed_args++;
            break;
        default:
            UNSUPPORTED("unsupported call parameter type (%u)", arg_type);
	}

	vtop--;
    }

    arg_register_type = vtop[0].r & VT_VALMASK;
    arg_type = vtop[0].type.t & VT_BTYPE;
    sym = vtop[0].sym;


    if (arg_register_type == VT_CONST && arg_type == VT_FUNC) {
        emit_jsr(sym, (uint16_t)vtop[0].c.ul);
    } else {
        UNSUPPORTED("unsupported call function type (%x, %x) %p", arg_register_type, arg_type, vtop[0].sym);
    }

    vtop--;

#ifdef USE_REGS_FOR_LOCALS
    // Restore regs.
    for (i = 1; i <= regs_to_save && i <= NUM_LOCAL_REGS; i++) {
         emit_read_stack(-i, DV_A + i);
    }
#endif

    if (nb_pushed_args > 0)
        emit_simple_math(ADD, DV_SP, 0, nb_pushed_args, true);
}


/*
    Output a symbol and patch all calls to it
*/

ST_FUNC void gsym_addr(int t, int addr)
{
    Log("%s: t=%d, a=%d", __func__, t, addr);

    int n = 0;
    short *ptr = NULL;
    Sym *sym = NULL;

    if (t)
        sym = get_sym_ref(&char_pointer_type, cur_text_section, addr, 0);

    while (t) {

        printf("%u \n", t);

        ptr = (short *)(cur_text_section->data + t);
        n = *ptr; /* next value */

	greloc(cur_text_section, sym, t, R_DCPU_16_ADDR);

        t = n;
        *ptr = 0;
    }
}


/*
    Resolve all relocs to the current location.

    Uses gsym_addr with the current location.
*/

ST_FUNC void gsym(int t)
{
    Log("%s: t=%d", __func__, t);
    gsym_addr(t, ind);
}


/*
    Generate a jump to a label.

    See emit_jump_to_symbol for more explination
    about the argument t.

*/

ST_FUNC int gjmp(int t)
{
    Log("%s: %u", __func__, t);
    return emit_jump_to_symbol(t);
}


/*
    Generate a jump to a fixed address.

    This needs to be patched.
*/

ST_FUNC void gjmp_addr(int a)
{
    Log("%s: %x", __func__, a);

    // I guess this routine is used for relative short
    // local jumps, for now just handle it as the general
    // case

    // place a zero there later the symbol will be added to it
    int t = emit_jump_to_symbol(0);

    gsym_addr(t, a);
}


/*
    Generate a test.

    inv - set 'inv' to invert test. Stack entry is popped.
    t - see emit_jump_to_symbol.
*/

ST_FUNC int gtst(int inv, int t)
{
    Log(__func__);
    int v;

    dump_svalue_printf(vtop);

    v = vtop->r & VT_VALMASK;

    if (v == VT_CMP) {
        UNSUPPORTED("can't performe this conditional jump (VT_CMP) %i %i", inv, t);
    } else if (v == VT_JMP || v == VT_JMPI) {
        UNSUPPORTED("can't performe this conditional jump (VT_JMP, VT_JMPI) %i %i", inv, t);
    } else {
        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
            /* constant jmp optimization */
            if ((vtop->c.i != 0) != inv)
                t = gjmp(t);
        } else {
            // I think we need to get the value on the stack
            // into a register, test it, and generate a branch
            // return the address of the branch, so it can be
            // later patched

            v = gv(RC_INT);	// get value into a reg

            emit_simple_math(inv ? LFE : LFN, v, 0, 0, true);
            t = emit_jump_to_symbol(t);
        }
    }

    vtop--;
    return t;
}


/*****************************************************************************
 *
 * Not implemented stuff!
 *
 */


/* convert integers to fp 't' type. Must handle 'int', 'unsigned int'
   and 'long long' cases. */
ST_FUNC void gen_cvt_itof(int t)
{
    Log(__func__);
}

/* convert fp to int 't' type */
ST_FUNC void gen_cvt_ftoi(int t)
{
    Log(__func__);
}

/* convert from one floating point type to another */
ST_FUNC void gen_cvt_ftof(int t)
{
    Log(__func__);
}

#if CONFIG_TCC_BCHECK
/* generate a bounded pointer addition */
ST_FUNC void gen_bounded_ptr_add(void)
{
    Log(__func__);
}

/* patch pointer addition in vtop so that pointer dereferencing is
   also tested */
ST_FUNC void gen_bounded_ptr_deref(void)
{
    Log(__func__);
}
#endif

/* generate a floating point operation 'v = t1 op t2' instruction. The
   two operands are guaranted to have the same floating point type */
/* XXX: need to use ST1 too */
ST_FUNC void gen_opf(int op)
{
    Log(__func__);
}

/* computed goto support */
ST_FUNC void ggoto(void)
{
    Log(__func__);
}
#endif
