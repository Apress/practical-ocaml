#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

// the math headers are needed for the sqrt function
#include <math.h>

CAMLprim value pythag(value _m,value _n)
{
  CAMLparam2(_m,_n);
  int m = Int_val(_m);
  int n = Int_val(_n);
  CAMLreturn(Val_int(sqrt((m*m)+(n*n))));
}

void throws_exception() 
{
  CAMLparam0();
  caml_failwith("I can't succeed");
  CAMLreturn0;
}

CAMLprim value example_new_prim(value strval,value intval, value floatval)
{
  CAMLparam3(strval,intval,floatval);
  CAMLlocal1(res);
  res = alloc_small(3,0);
  Store_field(res,0,strval);
  Store_field(res,1,intval);
  Store_field(res,2,floatval);
  CAMLreturn(res);
}

CAMLprim value example_add_prim(value primval,value intval,value floatval)
{
  CAMLparam3(intval,floatval,primval);
  CAMLlocal4(res,newstringval,newintval,newfloatval);
  res = alloc_small(3,0);
  int int_from_struct = Int_val(Field(primval,1));
  int int_from_val = Int_val(intval);
  double float_from_struct = Double_val(Field(primval,2));
  double float_from_val = Double_val(floatval);
  char *stringval = String_val(Field(primval,0));
  newstringval = caml_copy_string(stringval);
  newintval = Val_int((int_from_struct + int_from_val));
  newfloatval = caml_copy_double((float_from_struct + float_from_val));
  Store_field(res,0,newstringval);
  Store_field(res,1,newintval);
  Store_field(res,2,newfloatval);
  CAMLreturn(res);
}
