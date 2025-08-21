FUNCTION ZHRBR_FG001_002.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I9951) TYPE  P9951 OPTIONAL
*"     VALUE(IOPER) TYPE  PSPAR-ACTIO OPTIONAL
*"--------------------------------------------------------------------

  DATA ls_return TYPE bapireturn1 .
  DATA lv_key    TYPE bapipakey .
  IF ioper EQ 'DEL'.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '9951'
        number        = i9951-pernr
        validityend   = i9951-endda
        validitybegin = i9951-begda
        recordnumber  = i9951-seqnr
        record        = i9951
        operation     = ioper
      IMPORTING
        return        = ls_return
        key           = lv_key.

  ELSE.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '9951'
        number        = i9951-pernr
        validityend   = i9951-endda
        validitybegin = i9951-begda
        record        = i9951
        operation     = ioper
      IMPORTING
        return        = ls_return
        key           = lv_key.
  ENDIF.

ENDFUNCTION.
