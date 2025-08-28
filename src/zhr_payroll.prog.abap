*&---------------------------------------------------------------------*
*&  Include           ZHR_PAYROLL
*&---------------------------------------------------------------------*

FORM fuzpa01.
  "ay ortasında işyeri değişikliği veya ssk grubu değişmişse
*  LOOP AT wpbp WHERE massn = '02' AND ( massg = '01' or massg = '05' ).
  DATA : lv_btrtl TYPE btrtl,
         lv_kostl TYPE kostl,
         lv_betrg TYPE betrg.

  LOOP AT wpbp INTO DATA(ls_wpbp).
    IF ls_wpbp-btrtl NE lv_btrtl
      AND lv_btrtl IS NOT INITIAL.
      DATA(lv_flag) = 'X'.
    ENDIF.

    IF ls_wpbp-aktivjn EQ 'X'.

      IF ( ls_wpbp-kostl NE lv_kostl
       AND lv_kostl IS NOT INITIAL ) .
        DATA(lv_flag2) = 'X'.
      ENDIF.

      SELECT SINGLE bet01 FROM pa0008  INTO @DATA(lv_bet01)
                                       WHERE pernr EQ @pernr-pernr
                                         AND begda LE @ls_wpbp-endda
                                         AND endda GE @ls_wpbp-begda.
      IF lv_betrg NE lv_bet01
        AND lv_betrg IS NOT INITIAL.
        lv_flag2 = 'X'.
      ENDIF.

      lv_betrg = lv_bet01.
      lv_kostl = ls_wpbp-kostl.
    ENDIF.
    lv_btrtl = ls_wpbp-btrtl.
  ENDLOOP.

  IF lv_flag EQ 'X'.
    var-lgart = 'ZSPL'.
    var-anzhl = 1.
    APPEND var.  CLEAR var .

    var-lgart = 'ZSP2'.
    var-anzhl = 1.
    APPEND var.  CLEAR var .
  ENDIF.


  IF lv_flag2 EQ 'X'.
    var-lgart = 'ZSP2'.
    var-anzhl = 1.
    APPEND var.  CLEAR var .
  ENDIF.



  LOOP AT var WHERE lgart = 'ZSPL'
                AND anzhl NE 0.
  ENDLOOP.
  CHECK sy-subrc NE 0.
  LOOP AT p0769 WHERE pernr = pernr-pernr
                  AND begda <= aper-endda
                  AND endda >= aper-begda
                  AND bkodu = '02'.
  ENDLOOP.
  IF sy-subrc EQ 0.
    DATA(lv_lines) = lines( p0769[] ).
    IF lv_lines GT 1.
      LOOP AT p0769 INTO DATA(ls_p0769) FROM 1 TO lv_lines - 1.
        IF ls_p0769-bkodu NE p0769-bkodu.
          var-lgart = 'ZSPL'.
          var-anzhl = 1.
          APPEND var.  CLEAR var .

          var-lgart = 'ZSP2'.
          var-anzhl = 1.
          APPEND var.  CLEAR var .
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
