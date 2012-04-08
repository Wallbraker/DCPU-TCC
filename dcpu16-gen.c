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

ST_DATA const int reg_classes[NB_REGS] = {
    RC_INT | RC_A,
    RC_INT | RC_B,
    RC_INT | RC_C,
    RC_INT | RC_X,
    RC_INT | RC_Y,
    RC_INT | RC_Z,
    RC_INT | RC_I,
};

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
#define DINS_NUM_BASIC (IFB + 1)
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
    Emits code that reads SP + offset into any GP
    register except J (DV_A-DV_I, 0-6).
*/

void emit_read_stack(int offset, int reg)
{
    // SET J, SP
    emit_ins(SET, DV_J, 0, DV_SP, 0);

    if (offset < SHRT_MIN || offset > SHRT_MAX)
        tcc_error("ICE: stack access out of bounds!");

    // SET reg, [J+offset]
    emit_ins(SET, reg, 0, DV_REF_REG_NEXTWORD_BASE + DV_J, offset);
}


/*
    Emits code that writes to SP + offset from any GP
    register except J (DV_A-DV_I, 0-6).
*/

void emit_write_stack(int offset, int reg)
{
    // SET J, SP
    emit_ins(SET, DV_J, 0, DV_SP, 0);

    if (offset < SHRT_MIN || offset > SHRT_MAX)
        tcc_error("ICE: stack access out of bounds!");

    // SET reg, [J+offset]
    emit_ins(SET, DV_REF_REG_NEXTWORD_BASE + DV_J, offset, reg, 0);
}


/*
    Helper function to emit simple arithmatical
    opcodes on the form "a = a 'op' b". Handles
    constants either as a literal or a nextword.

    op - the operation to be used
    a - always a register offset
    b - either a register offset or a constant value
    b_is_const - b is stored as a literal or a nextword.


    1 word instruction to add a and b together.
    emit_simple_math(ADD, 0, 1, false);
    becomes
    emit_ins(ADD, DV_A, 0, DV_B, 0);


    1 word instruction to shift a by 5.
    emit_simple_math(SHL, 0, 5, true);
    becomes
    emit_ins(SHL, DV_A, 0, DV_LITERAL_BASE + 5, 0);


    2 word instruction to add 1337 to a.
    emit_simple_math(ADD, 0, 1337, true);
    becomes
    emit_ins(ADD, DV_A, 0, DV_NEXTWORD, 1337);
*/

void emit_simple_math(DIns op, int a, int b, bool b_is_const)
{
    if (b_is_const) {
        if (b < DV_LITERAL_NUM)
            emit_ins(op, DV_A + a, 0, DV_LITERAL_BASE + b, 0);
        else
            emit_ins(op, DV_A + a, 0, DV_NEXTWORD, b);
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
        emit_simple_math(SET, DV_PUSH, c, is_const);
    }
}


/*****************************************************************************
 *
 * Called by tcc.
 *
 */


/*
    Init this module.
*/

void gen_init()
{
    emit_ins = tcc_state->gen_asm ? i_emit_ins_ascii : i_emit_ins_binary;
}


/*
    load 'r' from value 'sv'
*/

ST_FUNC void load(int r, SValue *sv)
{
    Log("%s: %i %p (%i, %i, %i)", __func__, r, sv, sv->r, sv->type.t, sv->c.ul);

    bool pure_indirect;        // SET r, [sv]
    int regf = sv->r;          // flags & register
    int type_def = sv->type.t; // type definition
    int addr = sv->c.ul;       // address
    
    int align, size = type_size(&vtop[0].type, &align);

    int v = regf & VT_VALMASK;

    if (regf & VT_SYM)
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
            if (addr >= 0)
                addr = addr / 2 + 1; // Turn into stack reference.
            else
                addr = addr / 2;
            emit_read_stack(addr, DV_A + r);
        }
    } else {
        if (v == VT_CONST) {

            // SET r, value

            // Not really a address but thats where the data is.
            emit_simple_math(SET, r, addr, true);

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


    if (!(regf & VT_LVAL)) {
        UNSUPPORTED("can only store to VT_LVAL");
    }

    if (!(val_type == VT_INT || val_type == (VT_INT | VT_UNSIGNED) ||
          val_type == VT_SHORT || val_type == (VT_SHORT | VT_UNSIGNED) ||
          val_type == VT_BYTE || val_type == (VT_BYTE | VT_UNSIGNED) ||
          val_type == VT_PTR)) {
        UNSUPPORTED("unsupported format to store to (%u)", val_type);
    }


    if (v == VT_LOCAL) {

        // SET [SP + addr], r

        // Turn into stack reference.
        if (addr >= 0)
            addr = addr / 2 + 1;
        else
            addr = addr / 2;

        emit_write_stack(addr, DV_A + r);

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
    int r, fr, fc, r_basic_type;
    int size, align;
    bool f_is_const = false;

    Log(__func__);

    size = type_size(&vtop[0].type, &align);
    r = vtop[-1].r;
    r_basic_type = vtop[-1].type.t & VT_BTYPE;
    fr = vtop[0].r;
    fc = vtop[0].c.ul;

    // get the actual values
    if ((fr & VT_VALMASK) == VT_CONST) {
        // vtop is const, only need to load the other one
        vtop--;
        r = gv(RC_INT);

        if (size <= 2 && (fc < -32768 || fc > 65535))
            tcc_warning("large integer implicitly truncated");

        fr = (short)fc; // truncate and make it pass the constant around in fr.
        f_is_const = true;

        if (r_basic_type == VT_PTR) {
            if (fr % 2)
                UNSUPPORTED("Unaligned pointer access!\n");
            fr /= 2;
        }
    } else {
        gv2(RC_INT, RC_INT);

        // have to load both operands to registers
        r = vtop[-1].r;
        fr = vtop[0].r;

        if (r_basic_type == VT_PTR)
            UNSUPPORTED("Non constant operation on pointer\n");

        vtop--;
    }

    switch(op) {
    case TOK_ADDC2: // with carry use.
        emit_ins(ADD, DV_A + r, 0, DV_O, 0);
    case '+':
    case TOK_ADDC1: // with carry generation.
        emit_simple_math(ADD, r, fr, f_is_const);
        break;

    case TOK_SUBC2: // with carry use.
        emit_ins(SUB, DV_A + r, 0, DV_O, 0);
    case '-':
    case TOK_SUBC1: // with carry generation.
        emit_simple_math(SUB, r, fr, f_is_const);
        break;
    case '/':
        emit_simple_math(DIV, r, fr, f_is_const);
        break;
    case '*':
        emit_simple_math(MUL, r, fr, f_is_const);
        break;
    default:
        UNSUPPORTED("unsupported integer operation (%x)", op);
    }
}

ST_FUNC void gfunc_prolog(CType *func_type)
{
    int param_index, size, align, addr;
    Sym *sym;
    CType *type;

    Log(__func__);

    sym = func_type->ref;
    func_vt = sym->type;
    addr = 0;
    loc = 0;
    func_vc = 0;

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

        // We pass everything via the stack
        sym_push(sym->v & ~SYM_FIELD, type,
                VT_LOCAL | lvalue_type(type->t), addr);

        addr += size;
        param_index++;
    }
}


/*
    The end of the function, currently we only need to return.
*/

ST_FUNC void gfunc_epilog(void)
{
    Log(__func__);
    emit_ins(SET, DV_PC, 0, DV_POP, 0);
}


/*
    Generate function call. The function address is pushed first, then
    all the parameters in call order. This functions pops all the
    parameters and the function address.
*/

ST_FUNC void gfunc_call(int nb_args)
{
    int arg_type, arg_register_type, i, r, value;
    bool arg_is_const = false;
    Sym *sym = NULL;

    Log("%s: num args %u", __func__, nb_args);

    save_regs(0);

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

    // Need to clean up after the call
    if (nb_args > 0)
        emit_simple_math(ADD, DV_SP, nb_args, true);
}


/*****************************************************************************
 *
 * Not implemented stuff!
 *
 */


/* load 'r' from value 'sv' */
//ST_FUNC void load(int r, SValue *sv)
//{
//    Log(__func__);
//}

/* store register 'r' in lvalue 'v' */
//ST_FUNC void store(int r, SValue *v)
//{
//    Log(__func__);
//}

/* generate function prolog of type 't' */
//ST_FUNC void gfunc_prolog(CType *func_type)
//{
//    Log(__func__);
//}

/* generate function epilog */
//ST_FUNC void gfunc_epilog(void)
//{
//    Log(__func__);
//}

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

/* generate a jump to a label */
ST_FUNC int gjmp(int t)
{
    Log(__func__);
    return 0;
}

/* generate a jump to a fixed address */
ST_FUNC void gjmp_addr(int a)
{
    Log(__func__);
}

/* generate a test. set 'inv' to invert test. Stack entry is popped */
ST_FUNC int gtst(int inv, int t)
{
    Log(__func__);
    return 0;
}

/* generate an integer binary operation */
//ST_FUNC void gen_opi(int op)
//{
//    Log(__func__);
//}

/* generate a floating point operation 'v = t1 op t2' instruction. The
   two operands are guaranted to have the same floating point type */
/* XXX: need to use ST1 too */
ST_FUNC void gen_opf(int op)
{
    Log(__func__);
}

/* Generate function call. The function address is pushed first, then
   all the parameters in call order. This functions pops all the
   parameters and the function address. */
//ST_FUNC void gfunc_call(int nb_args)
//{
//    Log(__func__);
//}

/* output a symbol and patch all calls to it */
ST_FUNC void gsym_addr(int t, int a)
{
    Log("%s: t=%d, a=%d", __func__, t, a);
    int n, *ptr;
    while (t) {
        ptr = (int *)(cur_text_section->data + t);
        n = *ptr; /* next value */
        *ptr = a - t - 2;
        t = n;
    }
}

ST_FUNC void gsym(int t)
{
    Log("%s: t=%d", __func__, t);
    gsym_addr(t, ind);
}

/* computed goto support */
ST_FUNC void ggoto(void)
{
    Log(__func__);
}
#endif
