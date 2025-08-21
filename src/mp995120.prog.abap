*----------------------------------------------------------------------*
*                                                                      *
*       Output-modules for infotype 9951                               *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       MODULE  P9951 OUTPUT                                           *
*----------------------------------------------------------------------*
*       Default values, Texts                                          *
*----------------------------------------------------------------------*
MODULE p9951 OUTPUT.
  IF psyst-nselc EQ yes.
* read text fields etc.; do this whenever the screen is show for the
*  first time:
*   PERFORM RExxxx.
    IF psyst-iinit = yes AND psyst-ioper = insert.
* generate default values; do this the very first time on insert only:
*     PERFORM GET_DEFAULT.
    ENDIF.
  ENDIF.

ENDMODULE.                    "P9951 OUTPUT
*----------------------------------------------------------------------*
*       MODULE  P9951L OUTPUT                                          *
*----------------------------------------------------------------------*
*       read texts for listscreen
*----------------------------------------------------------------------*
MODULE p9951l OUTPUT.
* PERFORM RExxxx.
ENDMODULE.                    "P9951L OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.
  DATA : t9951 TYPE TABLE OF pa9951.
  DATA : s9951 TYPE pa9951.
  IF fcode EQ 'UPD'.
    SELECT * FROM pa9951 INTO TABLE t9951
                              WHERE pernr EQ p9951-pernr
                                AND begda EQ p9951-begda
                                AND endda EQ p9951-endda
                                AND icrid EQ p9951-icrid
                                AND manue NE 'X'.
    IF sy-subrc EQ 0.
      LOOP AT t9951 INTO s9951.
        DELETE FROM pa9951 WHERE pernr EQ p9951-pernr
                             AND begda EQ p9951-begda
                             AND endda EQ p9951-endda
                             AND icrid EQ p9951-icrid
                             AND manue NE 'X'.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDMODULE.                 " USER_COMMAND  INPUT
