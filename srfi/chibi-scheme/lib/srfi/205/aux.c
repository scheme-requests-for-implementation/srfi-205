// ~~~please see copyright notice in ./COPYING

#include <chibi/eval.h>

#include <stdlib.h>
#include <errno.h>
#include <sys/stat.h>
#include <termios.h>


// The Chibi Scheme implementation of errno manipulation is
// unsatisfactory because Chibi Scheme might set errno between any of
// 1) manually setting errno,
// 2) a POSIX call possibly setting it, and
// 3) retrieving its value.  See the SRFI 199 discussion for more
// details: (https://srfi-email.schemers.org/srfi-199/)

// ~~~set-errno sets errno to its supplied argument

sexp sexp_set_errno (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, x);
  errno = sexp_unbox_fixnum(x);

  return SEXP_VOID;
}


//~~~
sexp sexp_fileno_to_fd (sexp ctx, sexp self, sexp_sint_t n, sexp the_fileno) {
  sexp res;
  if (! (sexp_filenop(the_fileno)))
    return sexp_type_exception(ctx, self, SEXP_FILENO, the_fileno);
  res = sexp_make_integer(ctx, sexp_fileno_fd(the_fileno));
  return res;
}


sexp sexp_25_tcsetattr_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0, sexp arg1, sexp arg2) {
  int err = 0;
  sexp res;
  if (! (sexp_portp(arg0) || sexp_filenop(arg0) || sexp_fixnump(arg0)))
    return sexp_xtype_exception(ctx, self, "not a port or file descriptor",arg0);
  if (! sexp_exact_integerp(arg1))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg1);
  if (! (sexp_pointerp(arg2) && (sexp_pointer_tag(arg2) == sexp_unbox_fixnum(sexp_opcode_arg3_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg3_type(self)), arg2);
  err = tcsetattr((sexp_portp(arg0) ? sexp_port_fileno(arg0) : sexp_filenop(arg0) ? sexp_fileno_fd(arg0) : sexp_unbox_fixnum(arg0)), sexp_sint_value(arg1), (struct termios*)sexp_cpointer_value(arg2));
  if (err) {
  res = SEXP_FALSE;
  } else {
  res = SEXP_TRUE;
  }
  return res;
}

sexp sexp_25_tcgetattr_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  int err = 0;
  struct termios* tmp1;
  sexp res;
  sexp_gc_var1(res1);
  if (! (sexp_portp(arg0) || sexp_filenop(arg0) || sexp_fixnump(arg0)))
    return sexp_xtype_exception(ctx, self, "not a port or file descriptor",arg0);
  sexp_gc_preserve1(ctx, res1);
  tmp1 = (struct termios*) calloc(1, 1 + sizeof(tmp1[0]));
  err = tcgetattr((sexp_portp(arg0) ? sexp_port_fileno(arg0) : sexp_filenop(arg0) ? sexp_fileno_fd(arg0) : sexp_unbox_fixnum(arg0)), tmp1);
  if (err) {
  res = SEXP_FALSE;
  } else {
  res1 = sexp_make_cpointer(ctx, sexp_unbox_fixnum(sexp_opcode_arg2_type(self)), tmp1, SEXP_FALSE, 1);
  res = res1;
  }
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_termios_get_c_iflag (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_make_unsigned_integer(ctx, ((struct termios*)sexp_cpointer_value(x))->c_iflag);
}

sexp sexp_termios_set_c_iflag (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp v) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(v))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, v);
  ((struct termios*)sexp_cpointer_value(x))->c_iflag = sexp_uint_value(v);
  return SEXP_VOID;
}

sexp sexp_termios_get_c_oflag (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_make_unsigned_integer(ctx, ((struct termios*)sexp_cpointer_value(x))->c_oflag);
}

sexp sexp_termios_set_c_oflag (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp v) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(v))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, v);
  ((struct termios*)sexp_cpointer_value(x))->c_oflag = sexp_uint_value(v);
  return SEXP_VOID;
}

sexp sexp_termios_get_c_cflag (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_make_unsigned_integer(ctx, ((struct termios*)sexp_cpointer_value(x))->c_cflag);
}

sexp sexp_termios_set_c_cflag (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp v) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(v))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, v);
  ((struct termios*)sexp_cpointer_value(x))->c_cflag = sexp_uint_value(v);
  return SEXP_VOID;
}

sexp sexp_termios_get_c_lflag (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_make_unsigned_integer(ctx, ((struct termios*)sexp_cpointer_value(x))->c_lflag);
}

sexp sexp_termios_set_c_lflag (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp v) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(v))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, v);
  ((struct termios*)sexp_cpointer_value(x))->c_lflag = sexp_uint_value(v);
  return SEXP_VOID;
}

// x is the termios struct pointer, i is the array index for the value for the c_cc array element to return

sexp sexp_termios_get_cc_element (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp i) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(i))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, i);
// printf ("\n\ncc_c element 1 = %u, i = %ld\n\n", ((struct termios*)sexp_cpointer_value(x))->c_cc[1], sexp_unbox_fixnum(i));
  return sexp_make_unsigned_integer(ctx, ((struct termios*)sexp_cpointer_value(x))->c_cc[sexp_unbox_fixnum(i)]);
}

sexp sexp_termios_set_cc_element (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp v, sexp i) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  if (! sexp_exact_integerp(v))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, v);
  if (! sexp_exact_integerp(i))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, i);
  ((struct termios*)sexp_cpointer_value(x))->c_cc[sexp_unbox_fixnum(i)] = sexp_uint_value(v);
  return SEXP_VOID;
}


sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {
  sexp sexp_termios_type_obj;
  sexp_gc_var3(name, tmp, op);

  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;

  sexp_define_foreign(ctx, env, "set-errno", 1, sexp_set_errno); //~~~

  sexp_define_foreign(ctx, env, "%fileno-to-fd", 1, sexp_fileno_to_fd);

  // ~~~~ examine sexp_register_simple_type to create double timespect struct???

  sexp_gc_preserve3(ctx, name, tmp, op);

  name = sexp_c_string(ctx, "termios", -1);
  sexp_termios_type_obj = sexp_register_c_type(ctx, name, sexp_finalize_c_type);
  tmp = sexp_string_to_symbol(ctx, name);
  sexp_env_define(ctx, env, tmp, sexp_termios_type_obj);

  sexp_type_slots(sexp_termios_type_obj) = SEXP_NULL;
  sexp_push(ctx, sexp_type_slots(sexp_termios_type_obj), sexp_intern(ctx, "c_lflag", -1));
  sexp_push(ctx, sexp_type_slots(sexp_termios_type_obj), sexp_intern(ctx, "c_cflag", -1));
  sexp_push(ctx, sexp_type_slots(sexp_termios_type_obj), sexp_intern(ctx, "c_oflag", -1));
  sexp_push(ctx, sexp_type_slots(sexp_termios_type_obj), sexp_intern(ctx, "c_iflag", -1));
  // ~~~~ it looks like I might want to add the specialized c_cc getter and setter here,
  //      but they're not the simple one or two argument variety
  sexp_type_getters(sexp_termios_type_obj) = sexp_make_vector(ctx, SEXP_FOUR, SEXP_FALSE);
  sexp_type_setters(sexp_termios_type_obj) = sexp_make_vector(ctx, SEXP_FOUR, SEXP_FALSE);

  tmp = sexp_make_type_predicate(ctx, name, sexp_termios_type_obj);
  name = sexp_intern(ctx, "term-attrs?", 11);
  sexp_env_define(ctx, env, name, tmp);

  op = sexp_define_foreign(ctx, env, "cc-element-set!", 3, sexp_termios_set_cc_element);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg3_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
// Per the above, this seems to be part of a mechanism that doesn't work for this specialized setter
// and does not exist for %tcgetattr or %tcsetattr
//  if (sexp_vectorp(sexp_type_setters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_setters(sexp_termios_type_obj), SEXP_THREE, op);

  op = sexp_define_foreign(ctx, env, "cc-element-get", 2, sexp_termios_get_cc_element);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
// Per the above, this seems to be part of a mechanism that doesn't work for this specialized getter
// and does not exist for %tcgetattr or %tcsetattr
//  if (sexp_vectorp(sexp_type_getters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_getters(sexp_termios_type_obj), SEXP_THREE, op);

  op = sexp_define_foreign(ctx, env, "lflag-set!", 2, sexp_termios_set_c_lflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  if (sexp_vectorp(sexp_type_setters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_setters(sexp_termios_type_obj), SEXP_THREE, op);
  op = sexp_define_foreign(ctx, env, "lflag-get", 1, sexp_termios_get_c_lflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }
  if (sexp_vectorp(sexp_type_getters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_getters(sexp_termios_type_obj), SEXP_THREE, op);

  op = sexp_define_foreign(ctx, env, "cflag-set!", 2, sexp_termios_set_c_cflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  if (sexp_vectorp(sexp_type_setters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_setters(sexp_termios_type_obj), SEXP_TWO, op);
  op = sexp_define_foreign(ctx, env, "cflag-get", 1, sexp_termios_get_c_cflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }
  if (sexp_vectorp(sexp_type_getters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_getters(sexp_termios_type_obj), SEXP_TWO, op);

  op = sexp_define_foreign(ctx, env, "oflag-set!", 2, sexp_termios_set_c_oflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  if (sexp_vectorp(sexp_type_setters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_setters(sexp_termios_type_obj), SEXP_ONE, op);
  op = sexp_define_foreign(ctx, env, "oflag-get", 1, sexp_termios_get_c_oflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }
  if (sexp_vectorp(sexp_type_getters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_getters(sexp_termios_type_obj), SEXP_ONE, op);

  op = sexp_define_foreign(ctx, env, "iflag-set!", 2, sexp_termios_set_c_iflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  if (sexp_vectorp(sexp_type_setters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_setters(sexp_termios_type_obj), SEXP_ZERO, op);
  op = sexp_define_foreign(ctx, env, "iflag-get", 1, sexp_termios_get_c_iflag);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }
  if (sexp_vectorp(sexp_type_getters(sexp_termios_type_obj))) sexp_vector_set(sexp_type_getters(sexp_termios_type_obj), SEXP_ZERO, op);

  op = sexp_define_foreign(ctx, env, "%tcsetattr", 3, sexp_25_tcsetattr_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg3_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }

  op = sexp_define_foreign(ctx, env, "%tcgetattr", 1, sexp_25_tcgetattr_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_termios_type_obj));
  }

  sexp_gc_release3(ctx);

  return SEXP_VOID;
}
