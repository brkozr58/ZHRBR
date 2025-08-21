*----------------------------------------------------------------------*
*                                                                      *
*       Output-modules for infotype 9950                               *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       MODULE  P9950 OUTPUT                                           *
*----------------------------------------------------------------------*
*       Default values, Texts                                          *
*----------------------------------------------------------------------*
MODULE p9950 OUTPUT.
  IF psyst-nselc EQ yes.
* read text fields etc.; do this whenever the screen is show for the
*  first time:
*   PERFORM RExxxx.
    IF psyst-iinit = yes AND psyst-ioper = insert.
* generate default values; do this the very first time on insert only:
*     PERFORM GET_DEFAULT.
    ENDIF.
  ENDIF.
  IF p9950-icrid IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZHRBR001'
      IMPORTING
        number                  = p9950-icrid
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name(3) EQ 'TAB'.
      screen-input = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                    "P9950 OUTPUT
*----------------------------------------------------------------------*
*       MODULE  P9950L OUTPUT                                          *
*----------------------------------------------------------------------*
*       read texts for listscreen
*----------------------------------------------------------------------*
MODULE p9950l OUTPUT.
* PERFORM RExxxx.
ENDMODULE.                    "P9950L OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ORNG1_VALUES  INPUT
*&---------------------------------------------------------------------*
MODULE orng1_values INPUT.

  SELECT * FROM zhrbr_t003 INTO TABLE t003.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'P9950'
      fieldname  = 'ORNG1'
      searchhelp = 'ZHRBR_SH001'.

ENDMODULE.                 " ORNG1_VALUES  INPUT
*&---------------------------------------------------------------------*
*&      Module  ORNG2_VALUES  INPUT
*&---------------------------------------------------------------------*
MODULE orng2_values INPUT.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'P9950'
      fieldname  = 'ORNG2'
      searchhelp = 'ZHRBR_SH001'.

ENDMODULE.                 " ORNG2_VALUES  INPUT
*&---------------------------------------------------------------------*
*&      Module  ORNG3_VALUES  INPUT
*&---------------------------------------------------------------------*
MODULE orng3_values INPUT.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'P9950'
      fieldname  = 'ORNG3'
      searchhelp = 'ZHRBR_SH001'.

ENDMODULE.                 " ORNG2_VALUES  INPUT
*&---------------------------------------------------------------------*
*&      Module  ORNG4_VALUES  INPUT
*&---------------------------------------------------------------------*
MODULE orng4_values INPUT.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'P9950'
      fieldname  = 'ORNG4'
      searchhelp = 'ZHRBR_SH001'.

ENDMODULE.                 " ORNG2_VALUES  INPUT
*&---------------------------------------------------------------------*
*&      Module  ORNG5_VALUES  INPUT
*&---------------------------------------------------------------------*
MODULE orng5_values INPUT.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'P9950'
      fieldname  = 'ORNG5'
      searchhelp = 'ZHRBR_SH001'.

ENDMODULE.                 " ORNG2_VALUES  INPUT

*&SPWIZARD: OUTPUT MODULE FOR TS 'TABS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tabs_active_tab_set OUTPUT.
  tabs-activetab = g_tabs-pressed_tab.
  CASE g_tabs-pressed_tab.
    WHEN c_tabs-tab1.
      g_tabs-subscreen = '2100'.
    WHEN c_tabs-tab2.
      g_tabs-subscreen = '2200'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TABS_ACTIVE_TAB_SET OUTPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TABS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tabs_active_tab_get INPUT.
  fcode = sy-ucomm.
  CASE fcode.
    WHEN c_tabs-tab1.
      g_tabs-pressed_tab = c_tabs-tab1.
    WHEN c_tabs-tab2.
      g_tabs-pressed_tab = c_tabs-tab2.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TABS_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  P9950_2100  OUTPUT
*&---------------------------------------------------------------------*
MODULE p9950_2100 OUTPUT.
  SELECT * FROM pa9951 INTO CORRESPONDING FIELDS OF TABLE t9951
                            WHERE pernr EQ p9950-pernr
                              AND icrid EQ p9950-icrid.

  CLEAR: v_icraa , v_ibann.
  SELECT SINGLE icraa icaib FROM zhrbr_t006 INTO (v_icraa , v_ibann )
                                     WHERE icrad EQ p9950-icrad.
  IF p9950-waers IS INITIAL.
    p9950-waers = 'TRY'.
  ENDIF.
  IF p9950-aiban IS INITIAL.
    p9950-aiban = v_ibann.
  ENDIF.

  DATA lv_icrtr TYPE betrg.
  CLEAR lv_icrtr.
  CASE p9950-hspsk.
    WHEN 01.
      p9950-waers = 'TRY'.
      p9950-icrtr = p9950-tkstt.
    WHEN 02.
      IF p9950-icrtr IS NOT INITIAL AND p9950-tksts IS NOT INITIAL.
        lv_icrtr    = p9950-icrtr + p9950-faizt.
        p9950-tkstt = lv_icrtr / p9950-tksts.
      ENDIF.
    WHEN 03.
      IF p9950-icrtr IS NOT INITIAL AND p9950-tkstt IS NOT INITIAL.
        lv_icrtr    = p9950-icrtr + p9950-faizt.
        p9950-tksts = lv_icrtr / p9950-tkstt.
      ENDIF.
    WHEN 04.
  ENDCASE .

  CLEAR s591s.
  SELECT SINGLE * FROM t591s INTO s591s
                            WHERE sprsl EQ sy-langu
                              AND infty EQ '9950'
                              AND subty EQ p9950-subty.
  IF p9950-icrtr IS NOT INITIAL OR p9950-faizt IS NOT INITIAL.
    "Kesilen
    CALL METHOD zhrbr_cl001=>odenen_borc
      EXPORTING
        icrid = p9950-icrid
        begda = p9950-endda
        endda = p9950-endda
      IMPORTING
        icodm = v_kesil.
    v_kalan = ( p9950-icrtr + p9950-faizt ) - v_kesil.
    IF v_kalan LT 0.
      CLEAR v_kalan.
    ENDIF.
  ELSE.
    CLEAR : v_kesil , v_kalan.
  ENDIF.
ENDMODULE.                 " P9950_2100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  P9950_2200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE p9950_2200 OUTPUT.
ENDMODULE.                 " P9950_2200  OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TCONT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
*MODULE tcont_change_tc_attr OUTPUT.
*  DESCRIBE TABLE t9951 LINES tcont-lines.
*ENDMODULE.                    "TCONT_CHANGE_TC_ATTR OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_2200  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_2200 OUTPUT.
  CLEAR : t9951.
  SELECT * FROM pa9951 INTO CORRESPONDING FIELDS OF TABLE t9951
                            WHERE pernr EQ p9950-pernr
                              AND icrid EQ p9950-icrid.

  SORT t9951 DESCENDING BY begda.

  IF v_new EQ '2'.
    PERFORM kesinti_yarat USING '2'.
  ELSE.
    PERFORM kesinti_yarat USING '1'.
  ENDIF.
ENDMODULE.                 " STATUS_2200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  P9950_HIDE  OUTPUT
*&---------------------------------------------------------------------*
MODULE p9950_hide OUTPUT.
  "Group2 = 01 Toplam tutar alanları , 02 Oranlama alanları
  "Group2 = 03 Taksit Tutarı alanı   , 04 Taksit sayısı alanı
  LOOP AT SCREEN.
    CASE p9950-hspsk.
      WHEN 00.
        IF screen-group4 EQ '01'.
          screen-invisible = 1.
          screen-active    = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 01.
        IF screen-group2 EQ '02' OR
           screen-group2 EQ '01' OR
           screen-group2 EQ '04' .
          screen-invisible = 1.
          screen-active    = 0.
          MODIFY SCREEN.
        ELSEIF screen-group2 EQ '03'.
          screen-invisible = 0.
          screen-active    = 1.
          MODIFY SCREEN.
        ENDIF.
      WHEN 02.
        IF screen-group2 EQ '02'.
          screen-invisible = 1.
          screen-active    = 0.
          MODIFY SCREEN.
        ELSEIF screen-group2 EQ '01'
            OR screen-group2 EQ '04'.
          screen-invisible = 0.
          screen-active    = 1.
          MODIFY SCREEN.
        ELSEIF screen-group2 EQ '03'.
          screen-invisible = 0.
          screen-active    = 1.
          screen-input     = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 03.
        IF screen-group2 EQ '02'.
          screen-invisible = 1.
          screen-active    = 0.
          MODIFY SCREEN.
        ELSEIF screen-group2 EQ '01'
            OR screen-group2 EQ '03'.
          screen-invisible = 0.
          screen-active    = 1.
          MODIFY SCREEN.
        ELSEIF screen-group2 EQ '04'.
          screen-invisible = 0.
          screen-active    = 1.
          screen-input     = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 04.
        IF screen-group2 EQ '02'.
          screen-invisible = 0.
          screen-active    = 1.
          MODIFY SCREEN.
        ELSEIF screen-group2 EQ '03'
            OR screen-group2 EQ '04'.
          screen-invisible = 1.
          screen-active    = 0.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.


  ENDLOOP.
ENDMODULE.                 " P9950_HIDE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2200 INPUT.
  CASE fcode.
    WHEN 'CRKST'.
      v_new = '2'.
    WHEN 'ZDEL'.
      v_new = '1'.
      PERFORM zprocess_9951_del.
    WHEN 'ZSAVE'.
      v_new = '1'.
      PERFORM zprocess_9951.
    WHEN 'ZCANC'.
      v_new = '1'.
  ENDCASE.
ENDMODULE.                    "user_command_2200 INPUT
*&---------------------------------------------------------------------*
*& Form SET_YENI_KESINTI
*&---------------------------------------------------------------------*
FORM kesinti_yarat  USING    p_val.
  IF p_val EQ '1'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'NEW'.
        screen-invisible = 1.
        screen-active    = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'NEW'.
        screen-invisible = 0.
        screen-active    = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "kesinti_yarat
*&---------------------------------------------------------------------*
*& Form ZPROCESS_9951
*&---------------------------------------------------------------------*
FORM zprocess_9951 .
  DATA : s9951 TYPE p9951.
  DATA : return LIKE bapireturn1.
  IF ( ( p9950-icrtr + p9950-faizt ) NE 0 AND  ok9951-odmtr GE 0 ).
    IF v_kalan GE ok9951-odmtr OR p9950-hspsk EQ '01'.

      s9951-pernr = p9950-pernr.
      s9951-icrid = p9950-icrid.
      s9951-begda = ok9951-begda.
      s9951-endda = ok9951-begda.
      s9951-manue = 'X'.
      s9951-odmtr = ok9951-odmtr.
      s9951-acikl = ok9951-acikl.

      CALL FUNCTION 'ZHRBR_FG001_002' STARTING NEW TASK space
        EXPORTING
          i9951 = s9951
          ioper = 'INS'.

      IF return-type EQ 'E' OR return-type EQ 'A'.
        MESSAGE 'Aktarılamayan Kayıt' TYPE 'E'.
      ELSE.
        COMMIT WORK AND WAIT .
        WAIT UP TO 2 SECONDS.
        CLEAR ok9951.
      ENDIF .
    ELSE.
      MESSAGE 'Kalan borcundan fazla kesinti girilemez' TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE 'Lütfen toplam borç ve kesinti tutarının dolu olduğundan emin olun'
       TYPE 'E'.
  ENDIF.
ENDFORM.                    "zprocess_9951
*&---------------------------------------------------------------------*
*&      Form  BDC_DATA_PROG
*&---------------------------------------------------------------------*
FORM bdc_data_prog  USING    prog dynp dynb.
  CLEAR bdcdata.
  bdcdata-program  = prog.
  bdcdata-dynpro   = dynp.
  bdcdata-dynbegin = dynb.
  APPEND bdcdata.
ENDFORM.                    " BDC_DATA_PROG
*&---------------------------------------------------------------------*
*&      Form  BDC_DATA_SCRN
*&---------------------------------------------------------------------*
FORM bdc_data_scrn  USING  fnam fval date.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  IF date EQ 'X'.
    WRITE fval TO bdcdata-fval.
  ELSE.
    bdcdata-fval = fval.
  ENDIF.
  APPEND bdcdata.
ENDFORM.                    " BDC_DATA_SCRN
*&---------------------------------------------------------------------*
*&      Form  ZPROCESS_9951_DEL
*&---------------------------------------------------------------------*
FORM zprocess_9951_del .
  DATA : s9951 TYPE p9951.
  DATA : return LIKE bapireturn1.
  DATA : answer .
  READ TABLE t9951 WITH KEY field = 'X'.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar      = text-002
        text_question = text-001
      IMPORTING
        answer        = answer.
    IF answer EQ '1'.
      SELECT SINGLE * FROM pa9951 INTO CORRESPONDING FIELDS OF s9951
                                 WHERE pernr EQ p9950-pernr
                                   AND begda EQ t9951-begda
                                   AND endda EQ t9951-endda
                                   AND icrid EQ p9950-icrid
                                   AND seqnr EQ t9951-seqnr.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'ZHRBR_FG001_002' STARTING NEW TASK space
          EXPORTING
            i9951 = s9951
            ioper = 'DEL'.
        IF return-type EQ 'E' OR return-type EQ 'A'.
          MESSAGE 'Aktarılamayan Kayıt' TYPE 'E'.
        ELSE.
          COMMIT WORK AND WAIT .
          WAIT UP TO 2 SECONDS.
          CLEAR ok9951.
        ENDIF .
      ELSE.
        MESSAGE 'Kayıt Bulunamadı' TYPE 'E'.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE 'Lütfen bir kayıt seçiniz'  TYPE 'W'.
  ENDIF.

ENDFORM.                    " ZPROCESS_9951_DEL

*&SPWIZARD: OUTPUT MODULE FOR TC 'TCONT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tcont_change_tc_attr OUTPUT.
  DESCRIBE TABLE t9951 LINES tcont-lines.
ENDMODULE.                    "TCONT_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TCONT'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tcont_mark INPUT.
  DATA: g_tcont_wa2 LIKE LINE OF t9951.
  IF tcont-line_sel_mode = 1
  AND t9951-field = 'X'.
    LOOP AT t9951 INTO g_tcont_wa2
      WHERE field = 'X'.
      g_tcont_wa2-field = ''.
      MODIFY t9951
        FROM g_tcont_wa2
        TRANSPORTING field.
    ENDLOOP.
  ENDIF.
  MODIFY t9951
    FROM t9951
    INDEX tcont-current_line
    TRANSPORTING field.
ENDMODULE.                    "TCONT_MARK INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2100 INPUT.

  CASE fcode.
    WHEN 'ATTCH'.
      PERFORM zprocess_attach.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2100  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZPROCESS_ATTACH
*&---------------------------------------------------------------------*
FORM zprocess_attach .
  DATA : lv_key TYPE char18.

  lv_key = p9950-pernr && p9950-icrid.
*  lv_key = '1'.
  CALL FUNCTION 'ZHRBR_FG001_003' STARTING NEW TASK space
    EXPORTING
      i_key = lv_key.
*   IMPORTING
*     DCMNT_DRM       =
*            .


ENDFORM.                    " ZPROCESS_ATTACH
