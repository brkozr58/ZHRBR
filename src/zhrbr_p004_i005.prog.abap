*&---------------------------------------------------------------------*
*& Include          ZHRBR_P004_I005
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BATCH_INPUT
*&---------------------------------------------------------------------*
FORM batch_input TABLES t_rows  TYPE salv_t_row.
  DATA: s_rows    TYPE LINE OF salv_t_row.
  DATA: ls_mess   TYPE bapireturn1,
        ls_return TYPE bapireturn.
*  DATA: ls_p0015 TYPE p0015.
  DATA: ls_p9950 TYPE p9950.
  DATA: ls_p9951 TYPE p9951.
  DATA : lv_pernr TYPE persno.
  DATA : lv_begda TYPE begda.
  DATA : lv_endda TYPE endda.

  DATA : lt_p9950 TYPE TABLE OF p9950,
         ls_pa9950 TYPE p9950 .

  SORT gt_sabit BY pernr.
  SORT gt_yuzde BY pernr.
  SORT gt_kesnt BY pernr.



  CASE 'X'.
    WHEN rb_sabit.
      SELECT * FROM pa0000 INTO TABLE lt_p0000
                           FOR ALL ENTRIES IN gt_sabit
                           WHERE pernr EQ gt_sabit-pernr
                             AND massn EQ '10'.
    WHEN rb_yuzde.
      SELECT * FROM pa0000 INTO TABLE lt_p0000
                           FOR ALL ENTRIES IN gt_yuzde
                           WHERE pernr EQ gt_yuzde-pernr
                             AND massn EQ '10'.
    WHEN rb_kesnt.
      SELECT * FROM pa0000 INTO TABLE lt_p0000
                           FOR ALL ENTRIES IN gt_kesnt
                           WHERE pernr EQ gt_kesnt-pernr
                             AND massn EQ '10'.
      SELECT * FROM pa9950 INTO CORRESPONDING FIELDS OF TABLE lt_p9950
                           FOR ALL ENTRIES IN gt_kesnt
                              WHERE pernr EQ gt_kesnt-pernr
                                AND icrid EQ gt_kesnt-icrid .
  ENDCASE.

  LOOP AT t_rows INTO s_rows .
    CLEAR : ls_p9950, ls_p9951.
    CASE 'X'.
      WHEN rb_sabit.
        READ TABLE gt_sabit INTO gs_sabit INDEX s_rows.
        MOVE-CORRESPONDING: gs_sabit TO ls_p9950.
        lv_pernr = ls_p9950-pernr.
        lv_begda = ls_p9950-begda.
        lv_endda = ls_p9950-endda.
      WHEN rb_yuzde.
        READ TABLE gt_yuzde INTO gs_yuzde INDEX s_rows.
        MOVE-CORRESPONDING: gs_yuzde TO ls_p9950.
        lv_pernr = ls_p9950-pernr.
        lv_begda = ls_p9950-begda.
        lv_endda = ls_p9950-endda.
      WHEN rb_kesnt.
        READ TABLE gt_kesnt INTO gs_kesnt INDEX s_rows.
        MOVE-CORRESPONDING: gs_kesnt TO ls_p9951.
        lv_pernr = ls_p9951-pernr.
        lv_begda = ls_p9951-begda.
        lv_endda = ls_p9951-endda.

        READ TABLE lt_p9950 INTO ls_pa9950
          WITH KEY pernr = gs_kesnt-pernr
                   icrid = gs_kesnt-icrid.
        IF sy-subrc NE 0 .
          MOVE 'İlgili borç numarası bulunamadı!'
           TO gs_kesnt-messa.
          gs_kesnt-icon = '@8O@'.
          MODIFY gt_kesnt FROM gs_kesnt INDEX s_rows.
          CONTINUE.
        ENDIF.
    ENDCASE.

    LOOP AT lt_p0000 INTO ls_p0000
      WHERE pernr = lv_pernr
      AND   begda LE lv_endda
      AND   endda GE lv_begda.
    ENDLOOP.
    IF sy-subrc EQ 0.
      DATA : lv_msg(100) TYPE c.
      CONCATENATE ls_p0000-begda+6(2) '.' ls_p0000-begda+4(2) '.'
      ls_p0000-begda+0(4) INTO lv_msg.
      CONCATENATE lv_msg 'tarihinde işten çıkış kaydı bulunmaktadır.'
      INTO lv_msg SEPARATED BY space.

      CASE 'X'.
        WHEN rb_sabit.
          MOVE lv_msg TO gs_sabit-messa.
          gs_sabit-icon = '@8O@'.
          MODIFY gt_sabit FROM gs_sabit INDEX s_rows.
          CONTINUE.
        WHEN rb_yuzde.
          MOVE lv_msg TO gs_yuzde-messa.
          gs_yuzde-icon = '@8O@'.
          MODIFY gt_yuzde FROM gs_yuzde INDEX s_rows.
          CONTINUE.
        WHEN rb_kesnt.
          MOVE lv_msg TO gs_kesnt-messa.
          gs_kesnt-icon = '@8O@'.
          MODIFY gt_kesnt FROM gs_kesnt INDEX s_rows.
          CONTINUE.
      ENDCASE.

    ENDIF.

*---- Personel Lock
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_mess.

    IF ls_mess-type EQ 'E'.
      CASE 'X'.
        WHEN rb_sabit.
          MOVE ls_mess-message TO gs_sabit-messa.
          gs_sabit-icon = '@8O@'.
          MODIFY gt_sabit FROM gs_sabit INDEX s_rows.
          EXIT..
        WHEN rb_yuzde.
          MOVE ls_mess-message TO gs_yuzde-messa.
          gs_yuzde-icon = '@8O@'.
          MODIFY gt_yuzde FROM gs_yuzde INDEX s_rows.
          EXIT..
        WHEN rb_kesnt.
          MOVE ls_mess-message TO gs_kesnt-messa.
          gs_kesnt-icon = '@8O@'.
          MODIFY gt_kesnt FROM gs_kesnt INDEX s_rows.
          EXIT..
      ENDCASE.
    ENDIF.

    CASE 'X'.
      WHEN rb_sabit.
        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
          EXPORTING
            infty         = '9950'
            number        = ls_p9950-pernr
            subtype       = ls_p9950-subty
            validityend   = ls_p9950-endda
            validitybegin = ls_p9950-begda
            record        = ls_p9950
            operation     = 'INS'
          IMPORTING
            return        = ls_mess.
      WHEN rb_yuzde.
        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
          EXPORTING
            infty         = '9950'
            number        = ls_p9950-pernr
            subtype       = ls_p9950-subty
            validityend   = ls_p9950-endda
            validitybegin = ls_p9950-begda
            record        = ls_p9950
            operation     = 'INS'
          IMPORTING
            return        = ls_mess.
      WHEN rb_kesnt.
        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
          EXPORTING
            infty         = '9951'
            number        = ls_p9951-pernr
            subtype       = ls_p9951-subty
            validityend   = ls_p9951-endda
            validitybegin = ls_p9951-begda
            record        = ls_p9951
            operation     = 'INS'
          IMPORTING
            return        = ls_mess.
    ENDCASE.

    IF ls_mess-type EQ 'E'.
      CASE 'X'.
        WHEN rb_sabit.
          MOVE ls_mess-message TO gs_sabit-messa.
          gs_sabit-icon = '@8O@'.
          MODIFY gt_sabit FROM gs_sabit INDEX s_rows.
          EXIT..
        WHEN rb_yuzde.
          MOVE ls_mess-message TO gs_yuzde-messa.
          gs_yuzde-icon = '@8O@'.
          MODIFY gt_yuzde FROM gs_yuzde INDEX s_rows.
          EXIT..
        WHEN rb_kesnt.
          MOVE ls_mess-message TO gs_kesnt-messa.
          gs_kesnt-icon = '@8O@'.
          MODIFY gt_kesnt FROM gs_kesnt INDEX s_rows.
          EXIT..
      ENDCASE.
    ENDIF.

*------ Personel Unlock
    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = lv_pernr
      IMPORTING
        return = ls_mess.

    IF ls_mess-type EQ 'E'.
      CASE 'X'.
        WHEN rb_sabit.
          MOVE ls_mess-message TO gs_sabit-messa.
          gs_sabit-icon = '@8O@'.
          MODIFY gt_sabit FROM gs_sabit INDEX s_rows.
          EXIT..
        WHEN rb_yuzde.
          MOVE ls_mess-message TO gs_yuzde-messa.
          gs_yuzde-icon = '@8O@'.
          MODIFY gt_yuzde FROM gs_yuzde INDEX s_rows.
          EXIT..
        WHEN rb_kesnt.
          MOVE ls_mess-message TO gs_kesnt-messa.
          gs_kesnt-icon = '@8O@'.
          MODIFY gt_kesnt FROM gs_kesnt INDEX s_rows.
          EXIT..
      ENDCASE.
    ENDIF.


    CASE 'X'.
      WHEN rb_sabit.
        IF gs_sabit-icon IS INITIAL.
          gs_sabit-icon = '@2K@'.
          gs_sabit-messa = 'Başarılı'.
          MODIFY gt_sabit FROM gs_sabit INDEX s_rows TRANSPORTING messa icon.
        ENDIF.
      WHEN rb_yuzde.
        IF gs_yuzde-icon IS INITIAL.
          gs_yuzde-icon = '@2K@'.
          gs_yuzde-messa = 'Başarılı'.
          MODIFY gt_yuzde FROM gs_yuzde INDEX s_rows TRANSPORTING messa icon.
        ENDIF.
      WHEN rb_kesnt.
        IF gs_kesnt-icon IS INITIAL.
          gs_kesnt-icon = '@2K@'.
          gs_kesnt-messa = 'Başarılı'.
          MODIFY gt_kesnt FROM gs_kesnt INDEX s_rows TRANSPORTING messa icon.
        ENDIF.
    ENDCASE.
  ENDLOOP.
  COMMIT WORK.

ENDFORM.
