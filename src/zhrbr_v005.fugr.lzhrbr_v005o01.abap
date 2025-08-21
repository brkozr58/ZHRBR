*----------------------------------------------------------------------*
***INCLUDE LZHRBR_V005O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SET_TEXT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_text OUTPUT.

  CLEAR gs_text.
  SELECT SINGLE lgtxt FROM t512t INTO gs_text-lgart_t
                                WHERE sprsl EQ sy-langu
                                  AND molga EQ '47'
                                  AND lgart EQ zhrbr_v005-lgart.

  SELECT SINGLE butxt FROM t001 INTO gs_text-bukrs_t
                                WHERE bukrs EQ zhrbr_v005-bukrs.


ENDMODULE.                 " SET_TEXT  OUTPUT
