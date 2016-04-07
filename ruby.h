/************************************************

  ruby.h -

  $Author: matz $
  $Date: 1994/06/27 15:48:38 $
  created at: Thu Jun 10 14:26:32 JST 1993

  Copyright (C) 1994 Yukihiro Matsumoto

************************************************/

#ifndef RUBY_H
#define RUBY_H

#include "defines.h"

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>

#ifdef __STDC__
#else
#define volatile
#define const
#endif

#if defined(HAVE_ALLOCA_H)
#include <alloca.h>
#endif

typedef unsigned int UINT;
typedef UINT VALUE;
typedef UINT ID;
typedef unsigned short USHORT;
#ifdef __STDC__
# include <limits.h>
#else
# ifndef LONG_MAX
#  define LONG_MAX ((long)((unsigned long)~0L>>1))
# endif
# ifndef LONG_MIN
#  if (0 != ~0)
#   define LONG_MIN (-LONG_MAX-1)
#  else
#   define LONG_MIN (-LONG_MAX)
#  endif
# endif
#endif

#ifndef CHAR_BIT
# define CHAR_BIT 8
#endif

# define FIXNUM_MAX (LONG_MAX>>1)
# define FIXNUM_MIN RSHIFT((long)LONG_MIN,1)

#define FIXNUM_FLAG 0x01
#define INT2FIX(i) (VALUE)(((int)(i))<<1 | FIXNUM_FLAG)

#if (-1==(((-1)<<1)&FIXNUM_FLAG)>>1)
# define RSHIFT(x,y) ((x)>>y)
#else
# define RSHIFT(x,y) (((x)<0) ? ~((~(x))>>y) : (x)>>y)
#endif
#define FIX2INT(x) RSHIFT((int)x,1)

#define FIX2UINT(f) ((unsigned int)(f)>>1)
#define FIXNUM_P(f) (((int)(f))&FIXNUM_FLAG)
#define POSFIXABLE(f) ((f) <= FIXNUM_MAX)
#define NEGFIXABLE(f) ((f) >= FIXNUM_MIN)
#define FIXABLE(f) (POSFIXABLE(f) && NEGFIXABLE(f))

#define POINTER(p) (p)
#define NIL_P(p) ((p) == Qnil)

#define TRUE  INT2FIX(1)
#define FALSE Qnil

extern VALUE C_Object;
extern VALUE C_Nil;
extern VALUE C_Fixnum;
extern VALUE C_Data;

#define CLASS_OF(obj) (FIXNUM_P(obj)?C_Fixnum: NIL_P(obj)?C_Nil:\
                       RBASIC(obj)->class)

#define FL_SINGLE  0x10
#define FL_MARK    0x20
#define FL_LITERAL 0x40

#define FL_ABLE(x) (!(FIXNUM_P(x)||NIL_P(x)))
#define FL_TEST(x,f) (FL_ABLE(x)?(RBASIC(x)->flags&(f)):0)
#define FL_SET(x,f) if (FL_ABLE(x)) {RBASIC(x)->flags |= (f);}
#define FL_UNSET(x,f) if(FL_ABLE(x)){RBASIC(x)->flags &= ~(f);}

#define T_NIL    0x0
#define T_OBJECT 0x1
#define T_CLASS  0x2
#define T_ICLASS 0x3
#define T_MODULE 0x4
#define T_FLOAT  0x5
#define T_STRING 0x6
#define T_REGEXP 0x7
#define T_ARRAY  0x8
#define T_FIXNUM 0x9
#define T_DICT   0xA
#define T_DATA   0xB
#define T_METHOD 0xC
#define T_STRUCT 0xD
#define T_BIGNUM 0xE

#define T_MASK   0xF

#define BUILTIN_TYPE(x) (((struct RBasic*)(x))->flags & T_MASK)
#define TYPE(x) (FIXNUM_P(x)?T_FIXNUM:NIL_P(x)?T_NIL:BUILTIN_TYPE(x))
#define Check_Type(x,t) {if (TYPE(x)!=(t)) WrongType(x,t);}
#define Need_Fixnum(x)  {if (!FIXNUM_P(x)) (x) = num2fix(x);}
#define NUM2INT(x) (FIXNUM_P(x)?FIX2INT(x):num2int(x))
VALUE num2fix();
int   num2int();

#define NEWOBJ(obj,type) type *obj = (type*)newobj(sizeof(type))
#define OBJSETUP(obj,c,t) {\
    RBASIC(obj)->class = (c);\
    RBASIC(obj)->flags |= (t);\
}
#define CLONESETUP(obj1,obj2) \
    OBJSETUP(obj1,RBASIC(obj2)->class,RBASIC(obj2)->flags&T_MASK);
				 
struct RBasic {
    UINT flags;
    struct RBasic *next;
    VALUE class;
    struct st_table *iv_tbl;
};

struct RObject {
    struct RBasic basic;
};

struct RClass {
    struct RBasic basic;
    struct st_table *m_tbl;
    struct st_table *c_tbl;
    struct RClass *super;
};

struct RFloat {
    struct RBasic basic;
    double value;
};

struct RString {
    struct RBasic basic;
    UINT len;
    char *ptr;
    struct RString *orig;
};

struct RArray {
    struct RBasic basic;
    UINT len, capa;
    VALUE *ptr;
};

struct RRegexp {
    struct RBasic basic;
    struct Regexp *ptr;
    UINT len;
    char *str;
};

struct RDict {
    struct RBasic basic;
    struct st_table *tbl;
};

struct RData {
    struct RBasic basic;
    void (*dmark)();
    void (*dfree)();
    VALUE data[1];
};

#define DATA_PTR(dta) &(RDATA(dta)->data[0])

#define Get_Data_Struct(obj, iv, type, sval) {\
    VALUE _data_;\
    _data_ = rb_iv_get(obj, iv);\
    Check_Type(_data_, T_DATA);\
    sval = (type*)DATA_PTR(_data_);\
}

#define Make_Data_Struct(obj, iv, type, mark, free, sval) {\
    struct RData *_new_;\
    _new_ = (struct RData*)newobj(sizeof(struct RData)+sizeof(type));\
    OBJSETUP(_new_, C_Data, T_DATA);\
    _new_->dmark = (void (*)())(mark);\
    _new_->dfree = (void (*)())(free);\
    sval = (type*)DATA_PTR(_new_);\
    bzero(sval, sizeof(type));\
    rb_iv_set(obj, iv, _new_);\
}

struct RMethod {
    struct RBasic basic;
    struct node *node;
    struct RClass *origin;
    ID id;
    enum mth_scope { MTH_METHOD, MTH_FUNC, MTH_UNDEF } scope;
};

struct RStruct {
    struct RBasic basic;
    UINT len;
    struct kv_pair {
	ID key;
	VALUE value;
    } *tbl;
    char *name;
};

struct RBignum {
    struct RBasic basic;
    char sign;
    UINT len;
    USHORT *digits;
};

#define R_CAST(st) (struct st*)
#define RBASIC(obj)  (R_CAST(RBasic)(obj))
#define ROBJECT(obj) (R_CAST(RObject)(obj))
#define RCLASS(obj)  (R_CAST(RClass)(obj))
#define RFLOAT(obj)  (R_CAST(RFloat)(obj))
#define RSTRING(obj) (R_CAST(RString)(obj))
#define RREGEXP(obj) (R_CAST(RRegexp)(obj))
#define RARRAY(obj)  (R_CAST(RArray)(obj))
#define RDICT(obj)   (R_CAST(RDict)(obj))
#define RDATA(obj)   (R_CAST(RData)(obj))
#define RMETHOD(obj) (R_CAST(RMethod)(obj))
#define RSTRUCT(obj) (R_CAST(RStruct)(obj))
#define RBIGNUM(obj) (R_CAST(RBignum)(obj))

#define Qnil (VALUE)0

#define ALLOC_N(type,n) (type*)xmalloc(sizeof(type)*(n))
#define ALLOC(type) (type*)xmalloc(sizeof(type))
#define REALLOC_N(var,type,n) (var)=(type*)xrealloc((char*)(var),sizeof(type)*(n))

extern struct gc_list {
    int n;
    VALUE *varptr;
    struct gc_list *next;
} *GC_List;

#define GC_LINK { struct gc_list *_oldgc = GC_List;

#define GC_PRO(var) {\
    struct gc_list *_tmp = (struct gc_list*)alloca(sizeof(struct gc_list));\
    _tmp->next = GC_List;\
    _tmp->varptr = (VALUE*)&(var);\
    _tmp->n = 1;\
    GC_List = _tmp;\
}
#define GC_PRO2(var) GC_PRO3((var),Qnil)
#define GC_PRO3(var,init) {\
    (var) = (init);\
    GC_PRO(var);\
}
#define GC_PRO4(var,nelt) {\
    GC_PRO(var[0]);\
    GC_List->n = nelt;\
}

#define GC_UNLINK GC_List = _oldgc; }

VALUE rb_define_class();
VALUE rb_define_module();

void rb_define_variable();
void rb_define_const();

void rb_define_method();
void rb_define_func();
void rb_define_single_method();
void rb_define_mfunc();
void rb_undef_method();
void rb_define_alias();
void rb_define_attr();

ID rb_intern();
char *rb_id2name();

VALUE rb_funcall(VALUE recv, ID mid, int n, ...);
int rb_scan_args(VALUE args, char *fmt, ...);

VALUE rb_yield();

extern int verbose, debug;

/*** from cproto ***/
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include "st.h"
#include "node.h"
#include "re.h"
#include "io.h"
/* array.c */
VALUE ary_new2(int len);
VALUE ary_new(void);
VALUE ary_new3(int n, ...);
VALUE ary_new4(int n, VALUE *elts);
VALUE assoc_new(VALUE elm1, VALUE elm2);
VALUE Fary_push(struct RArray *ary, VALUE item);
VALUE Fary_pop(struct RArray *ary);
VALUE Fary_shift(struct RArray *ary);
VALUE Fary_unshift(struct RArray *ary, int item);
VALUE ary_entry(struct RArray *ary, int offset);
VALUE ary_join(struct RArray *ary, struct RString *sep);
VALUE Fary_to_s(VALUE ary);
VALUE Fary_sort(struct RArray *ary);
VALUE Fary_assoc(struct RArray *ary, VALUE key);
VALUE Fary_rassoc(struct RArray *ary, VALUE value);
int Init_Array(void);
/* bignum.c */
VALUE Fbig_clone(struct RBignum *x);
void big_2comp(struct RBignum *x);
VALUE bignorm(struct RBignum *x);
VALUE uint2big(UINT n);
VALUE int2big(int n);
VALUE uint2inum(UINT n);
VALUE int2inum(int n);
VALUE str2inum(char *str, int base);
VALUE big2str(struct RBignum *x, int base);
int big2int(struct RBignum *x);
VALUE Fbig_to_i(VALUE x);
VALUE dbl2big(double d);
double big2dbl(struct RBignum *x);
VALUE Fbig_to_f(VALUE x);
VALUE Fbig_plus(VALUE x, VALUE y);
VALUE Fbig_minus(VALUE x, VALUE y);
VALUE Fbig_mul(struct RBignum *x, struct RBignum *y);
VALUE Fbig_and(struct RBignum *x, struct RBignum *y);
VALUE Fbig_or(struct RBignum *x, struct RBignum *y);
VALUE Fbig_xor(struct RBignum *x, struct RBignum *y);
VALUE Fbig_lshift(struct RBignum *x, VALUE y);
int Init_Bignum(void);
/* class.c */
VALUE class_new(struct RClass *super);
VALUE single_class_new(struct RClass *super);
VALUE single_class_clone(struct RClass *class);
VALUE rb_define_class_id(ID id, struct RBasic *super);
VALUE rb_define_class(char *name, VALUE super);
VALUE module_new(void);
VALUE rb_define_module_id(ID id);
VALUE rb_define_module(char *name);
void rb_include_module(struct RClass *class, struct RClass *module);
void rb_add_method(struct RClass *class, ID mid, NODE *node, enum mth_scope scope);
void rb_define_method(struct RClass *class, char *name, VALUE (*func)(void), int argc);
void rb_define_func(struct RClass *class, char *name, VALUE (*func)(void), int argc);
void rb_undef_method(struct RClass *class, char *name);
VALUE rb_single_class(struct RBasic *obj);
void rb_define_single_method(VALUE obj, char *name, VALUE (*func)(void), int argc);
void rb_define_mfunc(struct RClass *class, char *name, VALUE (*func)(void), int argc);
void rb_define_alias(struct RClass *class, char *name1, char *name2);
void rb_define_attr(struct RClass *class, char *name, int pub);
void rb_define_single_attr(VALUE obj, char *name, int pub);
int rb_scan_args(VALUE args, char *fmt, ...);
/* compar.c */
int Init_Comparable(void);
/* dbm.c */
int Init_DBM(void);
/* dict.c */
VALUE Fdic_new(VALUE class);
VALUE Fdic_aset(struct RDict *dic, VALUE key, VALUE val);
VALUE Fgetenv(VALUE obj, struct RString *name);
VALUE Fsetenv(VALUE obj, struct RString *name, struct RString *value);
int Init_Dict(void);
/* dir.c */
int Init_Dir(void);
/* dln.c */
char *dln_find_exe(char *fname, char *path);
char *dln_find_file(char *fname, char *path);
int dln_init(char *file);
int dln_load(char *file);
int dln_load_lib(char *file);
/* enum.c */
void rb_each(VALUE obj);
int Init_Enumerable(void);
/* error.c */
int Error(char *fmt, ...);
int Warning(char *fmt, ...);
int Fatal(char *fmt, ...);
int Bug(char *fmt, ...);
int Fail(char *fmt, ...);
int rb_sys_fail(char *mesg);
int yyerror(char *msg);
int WrongType(VALUE x, int t);
/* etc.c */
int Init_Etc(void);
/* eval.c */
int main(int argc, char *argv[]);
VALUE TopLevel(char *script, int argc, char **argv);
void rb_trap_eval(VALUE cmd);
VALUE obj_responds_to(VALUE obj, struct RString *msg);
void rb_exit(int status);
VALUE Fexit(VALUE obj, VALUE args);
void rb_break(void);
void rb_redo(void);
void rb_retry(void);
void rb_fail(VALUE mesg);
VALUE Ffail(VALUE self, VALUE args);
int iterator_p(void);
VALUE rb_yield(VALUE val);
VALUE rb_iterate(VALUE (*it_proc)(void), char *data1, VALUE (*bl_proc)(void), char *data2);
VALUE rb_resque(VALUE (*b_proc)(void), char *data1, VALUE (*r_proc)(void), char *data2);
VALUE rb_ensure(VALUE (*b_proc)(void), char *data1, VALUE (*e_proc)(void), char *data2);
VALUE rb_apply(VALUE recv, ID mid, VALUE args);
VALUE Fapply(VALUE recv, VALUE args);
VALUE rb_funcall(VALUE recv, ID mid, int n, ...);
VALUE Fcaller(VALUE obj, VALUE args);
VALUE Feval(VALUE obj, struct RString *src);
VALUE Fload(VALUE obj, struct RString *fname);
int Frequire(VALUE obj, struct RString *fname);
int Init_load(void);
/* file.c */
VALUE file_open(char *fname, char *mode);
int cache_stat(char *path, struct stat *st);
int eaccess(char *path, int mode);
int Ffile_S(VALUE obj, struct RString *fname);
int Ffile_chown2(VALUE obj, VALUE owner, VALUE group);
int Ffile_readlink(VALUE obj, struct RString *path);
int Init_File(void);
/* gc.c */
void *xmalloc(unsigned long size);
void *xcalloc(unsigned long n, unsigned long size);
void *xrealloc(void *ptr, unsigned long size);
void rb_global_variable(VALUE *var);
VALUE Fgc_enable(void);
VALUE Fgc_disable(void);
VALUE Fgc_threshold(VALUE obj);
VALUE Fgc_set_threshold(VALUE obj, VALUE val);
struct RBasic *newobj(unsigned long size);
int literalize(struct RBasic *obj);
void unliteralize(struct RBasic *obj);
int gc(void);
int mark(register struct RBasic *obj);
int sweep(void);
int obj_free(struct RBasic *obj);
int Init_GC(void);
/* inits.c */
int rb_call_inits(void);
/* io.c */
void io_free_OpenFile(OpenFile *fptr);
int io_mode_flags(char *mode);
FILE *rb_fdopen(int fd, char *mode);
VALUE rb_check_str(VALUE val, ID id);
void io_ctl(VALUE obj, VALUE req, struct RString *arg, int io_p);
int Init_IO(void);
/* math.c */
int Init_Math(void);
/* methods.c */
NODE *rb_get_method_body(struct RClass *class, ID id, int envset, enum mth_scope scope);
void rb_alias(struct RClass *class, ID name, ID def);
void rb_clear_cache(struct RMethod *body);
void rb_clear_cache2(struct RClass *class);
/* numeric.c */
VALUE float_new(double flt);
int Fflo_pow(struct RFloat *x, struct RFloat *y);
int num2int(VALUE val);
VALUE num2fix(VALUE val);
VALUE Ffix_clone(VALUE num);
VALUE fix2str(VALUE x, int base);
VALUE Ffix_to_s(VALUE in);
int Init_Numeric(void);
/* object.c */
VALUE Fkrn_to_s(VALUE obj);
VALUE Fkrn_inspect(VALUE obj);
VALUE obj_is_member_of(VALUE obj, VALUE c);
VALUE obj_is_kind_of(VALUE obj, VALUE c);
VALUE obj_alloc(VALUE class);
int Init_Object(void);
/* pack.c */
int Init_pack(void);
/* process.c */
int rb_waitpid(int pid, int flags);
int rb_proc_exec(char *str);
void rb_syswait(int pid);
void mark_trap_list(void);
void rb_trap_exit(void);
int Fsleep(int argc, VALUE *argv);
int Init_process(void);
/* random.c */
int Init_Random(void);
/* range.c */
VALUE range_new(VALUE class, VALUE start, VALUE end);
int Init_Range(void);
/* re.c */
int str_cicmp(struct RString *str1, struct RString *str2);
Regexp *make_regexp(char *s, int len);
int research(struct RRegexp *reg, struct RString *str, int start, int ignorecase);
VALUE re_last_match(ID id);
int get_macth1(ID id);
int get_macth2(ID id);
int get_macth3(ID id);
int get_macth4(ID id);
int get_macth5(ID id);
int get_macth6(ID id);
int get_macth7(ID id);
int get_macth8(ID id);
int get_macth9(ID id);
void reg_free(Regexp *rp);
void reg_error(const char *s);
VALUE regexp_new(char *s, int len);
VALUE re_regcomp(struct RString *str);
VALUE Freg_match(struct RRegexp *re, struct RString *str);
VALUE Freg_match2(struct RRegexp *re);
VALUE re_regsub(struct RString *str);
void Init_Regexp(void);
/* regex.c */
long re_set_syntax(long syntax);
char *re_compile_pattern(char *pattern, size_t size, struct re_pattern_buffer *bufp);
void re_compile_fastmap(struct re_pattern_buffer *bufp);
int re_search(struct re_pattern_buffer *pbufp, char *string, int size, int startpos, int range, struct re_registers *regs);
int re_search_2(struct re_pattern_buffer *pbufp, char *string1, int size1, char *string2, int size2, int startpos, register int range, struct re_registers *regs, int mstop);
int re_match(struct re_pattern_buffer *pbufp, char *string, int size, int pos, struct re_registers *regs);
int re_match_2(struct re_pattern_buffer *pbufp, char *string1_arg, int size1, char *string2_arg, int size2, int pos, struct re_registers *regs, int mstop);
/* ruby.c */
void rb_load_file(char *fname);
void rb_main(int argc, char **argv);
/* socket.c */
/* sprintf.c */
VALUE Fsprintf(int argc, VALUE *argv);
/* st.c */
st_table *st_init_table_with_params(int (*compare)(void), int (*hash)(void), int size, int density, double grow_factor, int reorder_flag);
st_table *st_init_table(int (*compare)(void), int (*hash)(void));
int st_free_table(st_table *table);
int st_lookup(st_table *table, register char *key, char **value);
int st_insert(register st_table *table, register char *key, char *value);
int st_add_direct(st_table *table, char *key, char *value);
int st_find_or_add(st_table *table, char *key, char ***slot);
st_table *st_copy(st_table *old_table);
int st_delete(register st_table *table, register char **key, char **value);
int st_foreach(st_table *table, enum st_retval (*func)(void), char *arg);
int st_strhash(register char *string, int modulus);
/* string.c */
VALUE str_new(char *ptr, UINT len);
VALUE str_new2(char *ptr);
VALUE str_new3(struct RString *str);
VALUE obj_as_string(VALUE obj);
VALUE Fstr_clone(struct RString *str);
VALUE Fstr_plus(struct RString *str1, struct RString *str2);
VALUE Fstr_times(struct RString *str, VALUE times);
VALUE str_substr(struct RString *str, int start, int len);
VALUE str_subseq(struct RString *str, int beg, int end);
void str_modify(struct RString *str);
VALUE str_grow(struct RString *str, UINT len);
VALUE str_cat(struct RString *str, char *ptr, UINT len);
int str_cmp(struct RString *str1, struct RString *str2);
int Init_String(void);
/* struct.c */
VALUE struct_new(char *name, ...);
int Init_Struct(void);
/* time.c */
VALUE time_new(int sec, int usec);
struct timeval *time_timeval(VALUE time);
int Init_Time(void);
/* variable.c */
st_table *new_idhash(void);
void Init_var_tables(void);
void rb_name_class(VALUE class, ID id);
int mark_global_tbl(void);
struct global_entry *rb_global_entry(ID id);
void rb_define_variable(char *name, VALUE *var, VALUE (*get_hook)(void), VALUE (*set_hook)(void));
void rb_define_varhook(char *name, VALUE (*get_hook)(void), VALUE (*set_hook)(void));
VALUE rb_readonly_hook(VALUE val, ID id);
VALUE rb_id2class(ID id);
VALUE rb_gvar_get(struct global_entry *entry);
int rb_ivar_get_1(struct RBasic *obj, ID id);
VALUE rb_ivar_get(ID id);
VALUE rb_mvar_get(ID id);
VALUE rb_const_get(ID id);
VALUE rb_gvar_set(struct global_entry *entry, VALUE val);
VALUE rb_gvar_set2(char *name, VALUE val);
int rb_ivar_set_1(struct RBasic *obj, ID id, VALUE val);
VALUE rb_ivar_set(ID id, VALUE val);
VALUE rb_const_set(ID id, VALUE val);
void rb_define_const(struct RClass *class, char *name, VALUE val);
VALUE rb_iv_get(VALUE obj, char *name);
VALUE rb_iv_set(VALUE obj, char *name, VALUE val);
VALUE Fdefined(VALUE obj, struct RString *name);
/* version.c */
int Init_version(void);
int show_version(void);

#endif
