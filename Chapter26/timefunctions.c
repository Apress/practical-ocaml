#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h> 
#define __USE_XOPEN
#include <time.h>
#include <string.h>

#define TIMEBUF_LEN 40

void alloc_tm(value tm,struct tm *timestruct) {
  timestruct->tm_sec = Int_val(Field(tm, 0));
  timestruct->tm_min = Int_val(Field(tm, 1));
  timestruct->tm_hour = Int_val(Field(tm, 2));
  timestruct->tm_mday = Int_val(Field(tm, 3));
  timestruct->tm_mon = Int_val(Field(tm, 4));
  timestruct->tm_year = Int_val(Field(tm, 5));
  timestruct->tm_wday = Int_val(Field(tm, 6));
  timestruct->tm_yday = Int_val(Field(tm, 7));
  timestruct->tm_isdst = Bool_val(Field(tm, 8));
}

CAMLprim value caml_strftime(value timefmt,value tm)
{
  CAMLparam2(timefmt,tm);
  CAMLlocal1(formated_time);
  
  char *time_format = String_val(timefmt);
  char *strbuf = (char *)malloc(sizeof(' ')*TIMEBUF_LEN);
  struct tm timestruct;
  alloc_tm(tm,&timestruct);
  
  if ((strftime(strbuf,TIMEBUF_LEN,time_format,&timestruct)) == 0) {
    caml_failwith("strftime returned 0!");
  }
  
  formated_time = caml_copy_string(strbuf);
  free(strbuf);
  CAMLreturn(formated_time);
}

CAMLprim value caml_strptime(value timedata,value timefmt) 
{
  CAMLparam2(timedata,timefmt);
  CAMLlocal1(res);

  char *data = String_val(timedata);
  char *fmt = String_val(timefmt);
  char *err;

  struct tm timestruct;

  err = strptime(data,fmt,&timestruct);
  if (err == NULL) caml_failwith("stprtime failed");

  mktime(&timestruct);

  res = alloc_small(9, 0);
  Field(res,0) = Val_int(timestruct.tm_sec);
  Field(res,1) = Val_int(timestruct.tm_min);
  Field(res,2) = Val_int(timestruct.tm_hour);
  Field(res,3) = Val_int(timestruct.tm_mday);
  Field(res,4) = Val_int(timestruct.tm_mon);
  Field(res,5) = Val_int(timestruct.tm_year);
  Field(res,6) = Val_int(timestruct.tm_wday);
  Field(res,7) = Val_int(timestruct.tm_yday);
  Field(res,8) = timestruct.tm_isdst ? Val_true : Val_false;
  
  CAMLreturn(res);
}

CAMLprim value caml_asctime(value tm) {

  CAMLparam1(tm);
  CAMLlocal1(res);
  char *strbuf = (char *)malloc(sizeof(' ')*TIMEBUF_LEN);
  struct tm timestruct;
  alloc_tm(tm,&timestruct);

  char *ignore = asctime_r(&timestruct,strbuf);
  if ((strcmp(ignore,strbuf)) != 0) 
    caml_failwith("stprtime failed");    
  res = caml_copy_string(strbuf);
  free(strbuf);
  CAMLreturn(res);
}

CAMLprim value caml_difftime(value tm,value tm2) 
{
  CAMLparam2(tm,tm2);
  CAMLlocal1(res);

  struct tm timestruct;
  alloc_tm(tm,&timestruct);
  struct tm timestruct2;
  alloc_tm(tm2,&timestruct2);

  double diff = difftime(mktime(&timestruct),mktime(&timestruct2));
  res = caml_copy_double(diff);
  CAMLreturn(res);
}
  
