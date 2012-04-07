
typedef void (*DumpFunc)(const char*);


static inline void dump_ctype(DumpFunc df, CType *ct)
{
    int len = 0;
    char buffer[0x100];
    memset(buffer, 0, sizeof(buffer));

    int btype = ct->t & VT_BTYPE;
    int type = ct->t & VT_TYPE;

    len += sprintf(buffer + len, "\t\tbtype ");

    switch (btype) {
    case VT_INT:
        len += sprintf(buffer + len, "VT_INT\0");
        break;
    case VT_BYTE:
        len += sprintf(buffer + len, "VT_BYTE\0");
        break;
    case VT_SHORT:
        len += sprintf(buffer + len, "VT_SHORT\0");
        break;
    case VT_VOID:
        len += sprintf(buffer + len, "VT_VOID\0");
        break;
    case VT_PTR:
        len += sprintf(buffer + len, "VT_PTR\0");
        break;
    case VT_ENUM:
        len += sprintf(buffer + len, "VT_ENUM\0");
        break;
    case VT_FUNC:
        len += sprintf(buffer + len, "VT_FUNC\0");
        break;
    case VT_STRUCT:
        len += sprintf(buffer + len, "VT_STRUCT\0");
        break;
    case VT_FLOAT:
        len += sprintf(buffer + len, "VT_FLOAT\0");
        break;
    case VT_DOUBLE:
        len += sprintf(buffer + len, "VT_DOUBLE\0");
        break;
    case VT_LDOUBLE:
        len += sprintf(buffer + len, "VT_LDOUBLE\0");
        break;
    case VT_BOOL:
        len += sprintf(buffer + len, "VT_BOOL\0");
        break;
    case VT_LLONG:
        len += sprintf(buffer + len, "VT_LLONG\0");
        break;
    case VT_LONG:
        len += sprintf(buffer + len, "VT_LONG\0");
        break;
    default:
        len += sprintf(buffer + len, "???UNKNOWN???\0");
        break;
    }

    df(buffer);
    len = 0;

    len += sprintf(buffer + len, "\t\ttype ");
    if (type & VT_UNSIGNED)
        len += sprintf(buffer + len, "VT_UNSIGNED ");
    if (type & VT_ARRAY)
        len += sprintf(buffer + len, "VT_ARRAY ");
    if (type & VT_VLA)
        len += sprintf(buffer + len, "VT_VLA ");
    if (type & VT_BITFIELD)
        len += sprintf(buffer + len, "VT_BITFIELD ");
    if (type & VT_CONSTANT)
        len += sprintf(buffer + len, "VT_CONSTANT ");
    if (type & VT_VOLATILE)
        len += sprintf(buffer + len, "VT_VOLATILE ");
    if (type & VT_SIGNED)
        len += sprintf(buffer + len, "VT_SIGNED ");

    buffer[len] = 0;
    df(buffer);
    len = 0;
}


static inline void dump_r(DumpFunc df, int r)
{
    char buffer[0x100];
    int len = 0;

    int val = r & VT_VALMASK;

    len += sprintf(buffer + len, "\t\tr val ");

    switch (val) {
        break;
        len += sprintf(buffer + len, "VT_INT\0");
    case VT_CONST:
        len += sprintf(buffer + len, "VT_CONST\0");
        break;
    case VT_LLOCAL:
        len += sprintf(buffer + len, "VT_LLOCAL\0");
        break;
    case VT_LOCAL:
        len += sprintf(buffer + len, "VT_LOCAL\0");
        break;
    case VT_CMP:
        len += sprintf(buffer + len, "VT_CMP\0");
        break;
    case VT_JMP:
        len += sprintf(buffer + len, "VT_JMP\0");
        break;
    case VT_JMPI:
        len += sprintf(buffer + len, "VT_JMPI\0");
        break;
    default:
        if (val < VT_CONST)
            len += sprintf(buffer + len, "< VT_CONST (reg %u)\0", val);
        else
            len += sprintf(buffer + len, "???UNKOWN??? (%u)\0", val);
    }

    df(buffer);
    len = 0;

    len += sprintf(buffer + len, "\t\tr flags ");

    if (r & VT_LVAL)
        len += sprintf(buffer + len, "VT_LVAL ");
    if (r & VT_SYM)
        len += sprintf(buffer + len, "VT_SYM ");
    if (r & VT_MUSTCAST)
        len += sprintf(buffer + len, "VT_MUSTCAST ");
    if (r & VT_MUSTBOUND)
        len += sprintf(buffer + len, "VT_MUSTBOUND ");
    if (r & VT_BOUNDED)
        len += sprintf(buffer + len, "VT_BOUNDED ");
    if (r & VT_LVAL_BYTE)
        len += sprintf(buffer + len, "VT_LVAL_BYTE ");
    if (r & VT_LVAL_SHORT)
        len += sprintf(buffer + len, "VT_LVAL_SHORT ");
    if (r & VT_LVAL_UNSIGNED)
        len += sprintf(buffer + len, "VT_LVAL_UNSIGNED ");

    buffer[len] = 0;
    df(buffer);
    len = 0;
}


static inline void dump_svalue(DumpFunc df, SValue *v)
{
    char buffer[0x100];

    sprintf(buffer, "\tv->type.t: %x\0", v->type.t);
    df(buffer);

    dump_ctype(df, &(v->type));

    sprintf(buffer, "\tv->type.ref: %p\0", v->type.ref);
    df(buffer);
    sprintf(buffer, "\tv->r: %u\0", v->r);
    df(buffer);

    dump_r(df, v->r);

    sprintf(buffer, "\tv->r2: %u\0", v->r2);
    df(buffer);

    dump_r(df, v->r2);

    sprintf(buffer, "\tv->c.ul: 0x%04x\0", v->c.ul);
    df(buffer);
    sprintf(buffer, "\tv->sym: %p\0", v->sym);
    df(buffer);
}


static inline void dump_printf(const char*str)
{
    printf("%s\n", str);
}


static inline void dump_ctype_printf(CType *ct)
{
    dump_ctype(&dump_printf, ct);
}


static inline void dump_svalue_printf(SValue *v)
{
    dump_svalue(&dump_printf, v);
}

static inline void dump_r_printf(r)
{
    dump_r(&dump_printf, r);
}
