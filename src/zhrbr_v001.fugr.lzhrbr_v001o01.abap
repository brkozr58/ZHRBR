*----------------------------------------------------------------------*
***INCLUDE LZHRBR_V001O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SET_TEXT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_text OUTPUT.

CLEAR gs_text.
SELECT SINGLE stext FROM T591S INTO gs_text-subty_t
                              WHERE sprsl EQ sy-langu
                                AND infty EQ '9950'
                                AND subty EQ zhrbr_v001-subty.

SELECT SINGLE lgtxt FROM t512t INTO gs_text-kaluc_t
                              WHERE sprsl EQ sy-langu
                                AND molga EQ '47'
                                AND lgart EQ zhrbr_v001-kaluc.


SELECT SINGLE lgtxt FROM t512t INTO gs_text-ksluc_t
                              WHERE sprsl EQ sy-langu
                                AND molga EQ '47'
                                AND lgart EQ zhrbr_v001-ksluc.

ENDMODULE.                 " SET_TEXT  OUTPUT
