*----------------------------------------------------------------------*
***INCLUDE LZHRBR_V002O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SET_TEXT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_text OUTPUT.
  CLEAR gs_text.
  SELECT SINGLE stext FROM t591s INTO gs_text-subty_t
                                WHERE sprsl EQ sy-langu
                                  AND infty EQ '9950'
                                  AND subty EQ zhrbr_v002-subty.

  SELECT SINGLE butxt FROM t001 INTO gs_text-bukrs_t
                                WHERE bukrs EQ zhrbr_v002-bukrs.

ENDMODULE.                 " SET_TEXT  OUTPUT
