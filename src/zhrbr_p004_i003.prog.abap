*&---------------------------------------------------------------------*
*& Include          ZHRBR_P004_I003
*&---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    TYPES : gty_rows TYPE salv_t_row.
    METHODS :
      set_init,
      at_selection_screen ,
      screen_output,
      start_of_selection,
      set_data ,
      get_data_del,
      delete_rows
        IMPORTING lt_rows TYPE gty_rows,
      modify_screen,
*      refresh_alv,
      check_data ,
      set_texts,
      prepare_alv,
      display_alv,
      get_excel IMPORTING file_name TYPE any
                CHANGING  data      TYPE table.

  PROTECTED SECTION.
    DATA : gr_alv       TYPE REF TO cl_salv_table,
           gr_past      TYPE REF TO cl_salv_table,
           gr_display   TYPE REF TO cl_salv_display_settings,
           gr_columns   TYPE REF TO cl_salv_columns_table,
           gr_column    TYPE REF TO cl_salv_column_table,
           gr_functions TYPE REF TO cl_salv_functions_list,
           gr_selection TYPE REF TO  cl_salv_selections,
           gr_layout    TYPE REF TO cl_salv_layout,
           gr_events    TYPE REF TO cl_salv_events_table,
           gr_exp_msg   TYPE REF TO cx_salv_msg.

    DATA : gs_key   TYPE salv_s_layout_key.

  PRIVATE SECTION.
    METHODS :
      create_alv,
      set_pf_status,
      set_top_of_page,
      set_alv_properties,
      set_column_styles,
      set_column_text
        IMPORTING i_fname TYPE lvc_fname
                  i_text  TYPE any,
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      get_sample_excel_file ,
      check_file_name ,
      get_samp_exc_file_headers ,
      get_samp_exc_file_datas,
      filename_get .

ENDCLASS.                    "lcl_report DEFINITION

CLASS lcl_report IMPLEMENTATION.
  METHOD set_init.
    " fonksyon tuşuna isim verme
    sc_fc01-icon_id          = '@49@' .
    sc_fc01-quickinfo        = TEXT-001 .
    sc_fc01-icon_text        = TEXT-001 .

    sscrfields-functxt_01    = sc_fc01.
  ENDMETHOD.                    "set_init
  METHOD modify_screen.
    LOOP AT SCREEN.
      IF rb_ins = 'X'.
        IF screen-group1 = '02'.
          screen-active = 0.
        ENDIF.
      ELSE.
        IF screen-group1 = '01'.
          screen-active = 0.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.
  METHOD at_selection_screen.
    " selection screen kısmındaki fonksyonel tuşlara basılması
    IF sscrfields-ucomm EQ 'FC01'.
      " Örnek dosyanın oluşturulması
      me->get_sample_excel_file( ).
    ELSE.
      " Dosya kontrolü
      me->check_file_name( ).
    ENDIF.
  ENDMETHOD.                    "at_selection_screen
  METHOD get_sample_excel_file.
    DATA : subrc TYPE sy-subrc .
    DATA : fname(128) .
    DATA: ld_filename TYPE string,
          ld_path     TYPE string,
          ld_fullpath TYPE string,
          ld_result   TYPE i.

    DEFINE gui_download.
      " Excel formatında kaydı
      CONCATENATE ld_path &2 INTO ld_fullpath .
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename              = ld_fullpath
          filetype              = 'ASC'
*         APPEND                = 'X'
          write_field_separator = 'X'
          confirm_overwrite     = 'X'
        TABLES
          data_tab              = &1     "need to declare and
          fieldnames            = gt_head[]
        EXCEPTIONS
          file_open_error       = 1
          file_write_error      = 2
          OTHERS                = 3.
    END-OF-DEFINITION.

    " dosyanın oluşturulması
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
*       window_title      = ' '
        default_extension = 'XLS'
        default_file_name = 'HR_BORCLAR.xls'
        initial_directory = 'C:\'
      CHANGING
        filename          = ld_filename
        path              = ld_path
        fullpath          = ld_fullpath
        user_action       = ld_result.

    IF sy-subrc <> 0.
      MESSAGE TEXT-e02 TYPE 'E'.
    ELSE.
      gv_sample_file = ld_fullpath.
    ENDIF.

    " dosyanın içine baslık ve verilerin atılması
    IF gv_sample_file IS NOT INITIAL.
      CLEAR : gt_head, gs_head.
      " başlık bilgileri
      me->get_samp_exc_file_headers( ).
      " veriler
      REFRESH : lt_sabit,lt_yuzde, lt_kesnt.
      me->get_samp_exc_file_datas( ).

      CASE 'X'.
        WHEN rb_sabit.
          gui_download : lt_sabit[] 'Sabit_Tutar.xls'.
        WHEN rb_yuzde.
          gui_download : lt_yuzde[] 'Yuzde_Tutar.xls'.
        WHEN rb_kesnt.
          gui_download : lt_kesnt[] 'Kesintiler.xls'.
      ENDCASE.

    ENDIF.

  ENDMETHOD.
  METHOD get_samp_exc_file_headers.
    DEFINE add_head.
      gs_head-header = &1.
      APPEND gs_head TO gt_head.
      CLEAR  gs_head .
    END-OF-DEFINITION.
    REFRESH gt_head.
    CASE 'X'.
      WHEN rb_sabit.
        add_head : 'Personel Numarası',
                   'Alt Tip'  ,
                   'Başlangıç Tarihi'	,
                   'Bitiş Tarihi'	,
                   'Dosya Sıra No'  ,
                   'Dosya No'	,
                   'Kesinti Sırası'	,
                   'İcra Dairesi'  ,
                   'Alıcı'  ,
                   'Alıcı İban'	,
                   'Hesaplama Şekli'  ,
                   'Taksit Tutarı'  ,
                   'Para Birimi'  ,
                   'Borç Açıklaması'.

      WHEN rb_yuzde.
        add_head : 'Personel Numarası',
                   'Alt Tip',
                   'Başlangıç Tarihi',
                   'Bitiş Tarihi',
                   'Dosya Sıra No',
                   'Dosya No',
                   'Kesinti Sırası',
                   'İcra Dairesi'  ,
                   'Alıcı',
                   'Alıcı İban',
                   'Hesaplama Şekli',
                   'Borç Tutarı',
                   'Para Birimi',
                   'Faiz tutarı',
                   'Kesinti Oranı',
                   'Yüzde',
                   'Kesinti Oranı',
                   'Yüzde',
*                   'Kesinti Oranı',
*                   'Yüzde',
*                   'Kesinti Oranı',
*                   'Yüzde',
                   'Borç Açıklaması'.

      WHEN rb_kesnt.
        add_head : 'Personel Numarası',
                   'Alt Tip',
                   'Başlangıç Tarihi',
                   'Bitiş Tarihi',
                   'Borç ID',
                   'Ödeme Ttuarı',
                   'Manuel Ödeme'.

    ENDCASE.

  ENDMETHOD.
  METHOD get_samp_exc_file_datas.


     CASE 'X'.
      WHEN rb_sabit.
        ls_sabit-pernr = ''."'Personel Numarası',
        ls_sabit-subty = ''."'Alt Tip'  ,
        ls_sabit-begda = ''."'Başlangıç Tarihi' ,
        ls_sabit-endda = ''."'Bitiş Tarihi' ,
        ls_sabit-dossr = ''."'Dosya Sıra No'  ,
        ls_sabit-dosno = ''."'Dosya No' ,
        ls_sabit-kessr = ''."'Kesinti Sırası' ,
        ls_sabit-icrad = ''."'İcra Dairesi'  ,
        ls_sabit-alici = ''."'Alıcı'  ,
        ls_sabit-aiban = ''."'Alıcı İban' ,
        ls_sabit-hspsk = ''."'Hesaplama Şekli'  ,
        ls_sabit-tkstt = ''."'Taksit Tutarı'  ,
        ls_sabit-waers = ''."'Para Birimi'  ,
        ls_sabit-acilm = ''."'Borç Açıklaması'.
        APPEND ls_sabit TO lt_sabit.

      WHEN rb_yuzde.
        ls_yuzde-pernr = ''."'Personel Numarası',
        ls_yuzde-subty = ''."'Alt Tip',
        ls_yuzde-begda = ''."'Başlangıç Tarihi',
        ls_yuzde-endda = ''."'Bitiş Tarihi',
        ls_yuzde-dossr = ''."'Dosya Sıra No',
        ls_yuzde-dosno = ''."'Dosya No',
        ls_yuzde-kessr = ''."'Kesinti Sırası',
        ls_yuzde-icrad = ''."'İcra Dairesi'  ,
        ls_yuzde-alici = ''."'Alıcı',
        ls_yuzde-aiban = ''."'Alıcı İban',
        ls_yuzde-hspsk = ''."'Hesaplama Şekli',
        ls_yuzde-icrtr = ''."'Borç Tutarı',
        ls_yuzde-waers = ''."'Para Birimi',
        ls_yuzde-faizt = ''."'Faiz tutarı',
        ls_yuzde-orng1 = ''."'Kesinti Oranı',
        ls_yuzde-orny1 = ''."'Yüzde',
        ls_yuzde-orng2 = ''."'Kesinti Oranı',
        ls_yuzde-orny2 = ''."'Yüzde',
        ls_yuzde-acilm = ''."'Borç Açıklaması'.
        APPEND ls_yuzde TO lt_yuzde.
      WHEN rb_kesnt.

        ls_kesnt-pernr = ''."'Personel Numarası',
        ls_kesnt-subty = ''."'Alt Tip',
        ls_kesnt-begda = ''."'Başlangıç Tarihi',
        ls_kesnt-endda = ''."'Bitiş Tarihi',
        ls_kesnt-icrid = ''."'Borç ID',
        ls_kesnt-odmtr = ''."'Ödeme Ttuarı',
        ls_kesnt-manue = ''."'Manuel Ödeme'.
        APPEND ls_kesnt TO lt_kesnt.


    ENDCASE.





  ENDMETHOD.
  METHOD check_file_name.
    " dosyanın ismi girildi mi?
    IF p_file IS INITIAL.
      MESSAGE TEXT-e01 TYPE 'E'.
      EXIT.
    ENDIF.
  ENDMETHOD.
  METHOD filename_get.
    DATA : filetable TYPE filetable,
           wa        TYPE file_table,
           rc        TYPE i.
    CLEAR p_file.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      CHANGING
        file_table              = filetable
        rc                      = rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.

    CHECK sy-subrc EQ 0.
    LOOP AT filetable INTO wa.
      p_file = wa-filename.
    ENDLOOP.
  ENDMETHOD.
  METHOD check_data.
    DATA: ls_mess   TYPE bapireturn1,
          ls_return TYPE bapireturn.
    DATA: ls_p0015 TYPE p0015.
    DATA : lv_pernr TYPE persno .

      CASE 'X'.
        WHEN rb_sabit.
          LOOP AT  gt_sabit INTO gs_sabit .
            CALL FUNCTION 'BAPI_EMPLOYEE_CHECKEXISTENCE'
              EXPORTING
                number = gs_sabit-pernr
              IMPORTING
                return = ls_return.
            IF ls_return-type EQ 'E'.
              MOVE ls_return-message TO gs_sabit-messa.
              gs_sabit-icon = '@8O@'.
              MODIFY gt_sabit FROM gs_sabit TRANSPORTING messa icon
                                              WHERE pernr EQ gs_sabit-pernr.
            ENDIF.

          ENDLOOP.
        WHEN rb_yuzde.
          LOOP AT  gt_yuzde INTO gs_yuzde .
            CALL FUNCTION 'BAPI_EMPLOYEE_CHECKEXISTENCE'
              EXPORTING
                number = gs_yuzde-pernr
              IMPORTING
                return = ls_return.
            IF ls_return-type EQ 'E'.
              MOVE ls_return-message TO gs_yuzde-messa.
              gs_yuzde-icon = '@8O@'.
              MODIFY gt_yuzde FROM gs_yuzde TRANSPORTING messa icon
                                              WHERE pernr EQ gs_yuzde-pernr.
            ENDIF.

          ENDLOOP.
        WHEN rb_kesnt.
          LOOP AT  gt_kesnt INTO gs_kesnt .
            CALL FUNCTION 'BAPI_EMPLOYEE_CHECKEXISTENCE'
              EXPORTING
                number = gs_kesnt-pernr
              IMPORTING
                return = ls_return.
            IF ls_return-type EQ 'E'.
              MOVE ls_return-message TO gs_kesnt-messa.
              gs_kesnt-icon = '@8O@'.
              MODIFY gt_kesnt FROM gs_kesnt TRANSPORTING messa icon
                                              WHERE pernr EQ gs_kesnt-pernr.
            ENDIF.
          ENDLOOP.
      ENDCASE.


  ENDMETHOD.

  METHOD set_texts.
*    IF rb_ins EQ abap_true.
*      CALL FUNCTION 'ZHRIGA_FG036_001'
*        CHANGING
*          textt = gt_txt.
*
*      LOOP AT gt_txt INTO gs_txt.
*        CASE gs_txt-flag.
*          WHEN 'LGART'.
*            gs_report-lgtxt = gs_txt-txt01.
*            MODIFY gt_report FROM gs_report TRANSPORTING lgtxt
*                                            WHERE lgart EQ gs_txt-val01.
*          WHEN 'PREAS'.
*            gs_report-rtext = gs_txt-txt01.
*            MODIFY gt_report FROM gs_report TRANSPORTING rtext
*                                            WHERE preas EQ gs_txt-val01.
*        ENDCASE.
*        CLEAR gs_report.
*      ENDLOOP.
*    ENDIF.

  ENDMETHOD.

  METHOD prepare_alv.
    me->create_alv( ).
    me->set_pf_status( ).
    me->set_alv_properties( ).
    me->set_top_of_page( ).
    me->display_alv( ).
  ENDMETHOD.                    "prepare_alv

  METHOD create_alv.
    FIELD-SYMBOLS : <table> TYPE table.
    TRY.


      CASE 'X'.
        WHEN rb_sabit.
          ASSIGN gt_sabit[] TO <table>.
        WHEN rb_yuzde.
          ASSIGN gt_yuzde[] TO <table>.
        WHEN rb_kesnt.
          ASSIGN gt_kesnt[] TO <table>.
      ENDCASE.

        cl_salv_table=>factory(
                  IMPORTING
                    r_salv_table = gr_alv
                  CHANGING
                    t_table      = <table> ).
      CATCH
        cx_salv_msg INTO gr_exp_msg.
    ENDTRY.

  ENDMETHOD.                    "create_alv

  METHOD set_pf_status.
    gr_alv->set_screen_status(
                pfstatus      = 'GUI'
                report        = sy-repid
                set_functions = gr_alv->c_functions_all ).

    " Button gizleme
*    DATA(lo_func) = gr_alv->get_functions_base( ).
*    DATA(lt_func_list) = lo_func->get_functions( ).
**    break mcelen.
*    LOOP AT lt_func_list INTO DATA(ls_function) .
*      IF rb_ins = abap_true.
*        CHECK ls_function-r_function->get_name( ) = '&DELETE'.
*        ls_function-r_function->set_visible( abap_false ).
*        EXIT.
*      ELSE.
*        CHECK ls_function-r_function->get_name( ) = '&BATCH'.
*        ls_function-r_function->set_visible( abap_false ).
*        EXIT.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.                    "set_pf_status

  METHOD set_top_of_page.

    DATA : lo_header      TYPE REF TO cl_salv_form_layout_grid,
           lo_grid_bottom TYPE REF TO cl_salv_form_layout_grid,
           lo_logo        TYPE REF TO cl_salv_form_layout_logo,
           lo_text        TYPE REF TO cl_salv_form_text,
           lo_label       TYPE REF TO cl_salv_form_label.

    DATA : lv_date  TYPE text10.

    CREATE OBJECT lo_header.

    lo_header->create_header_information(
                    row    = 1
                    column = 1
                    text   = TEXT-t01 )  . " Rapor ismi
    lo_header->add_row( ).

    lo_grid_bottom = lo_header->create_grid(
                   row    = 3
                   column = 1 ).

    lo_label = lo_grid_bottom->create_label(
                   row     = 1
                   column  = 1
                   text    = TEXT-t02
                   tooltip = TEXT-t02 ). " Tarih

    WRITE sy-datum TO lv_date DD/MM/YYYY.
    lo_text = lo_grid_bottom->create_text(
                   row     = 1
                   column  = 2
                   text    = lv_date
                   tooltip = lv_date ). " Tarih

    lo_label = lo_grid_bottom->create_label(
                   row     = 2
                   column  = 1
                   text    = TEXT-t03
                   tooltip = TEXT-t03 ). " Kullanıcı

    lo_text = lo_grid_bottom->create_text(
                   row     = 2
                   column  = 2
                   text    = sy-uname
                   tooltip = sy-uname ). " Kullanıcı


    lo_label->set_label_for( lo_text ).

    CREATE OBJECT lo_logo.
    lo_logo->set_left_content( lo_header ).
    lo_logo->set_right_logo( '' ).

    gr_alv->set_top_of_list( lo_logo ).

  ENDMETHOD.                    "set_top_of_page

  METHOD set_alv_properties.

    gr_display = gr_alv->get_display_settings( ).

* Zebra sytle..
    gr_display->set_striped_pattern( cl_salv_display_settings=>true ).

    gr_columns = gr_alv->get_columns( ).

* Set optimize..
    gr_columns->set_optimize( abap_true ).

    gr_layout = gr_alv->get_layout( ).

* Set variant..
    gs_key-report = sy-repid.
    gr_layout->set_key( gs_key ).
    gr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

* Alv default settings..
    gr_layout->set_default( if_salv_c_bool_sap=>true ).

* Set selection..
    gr_selection = gr_alv->get_selections( ).
    gr_selection->set_selection_mode( if_salv_c_selection_mode=>cell ).

    gr_events = gr_alv->get_event( ).
* Set ALV Events.
    SET HANDLER gr_report->on_user_command FOR gr_events.

* Set Column Texts.
    me->set_column_text( i_fname = 'ICON'    i_text = TEXT-002 ).
    me->set_column_text( i_fname = 'MESSA'   i_text = TEXT-003 ).
    me->set_column_styles( ).

** Hide columns.
    gr_column ?= gr_columns->get_column( 'MARK' ).
    gr_column->set_technical( if_salv_c_bool_sap=>true ).

  ENDMETHOD.                    "set_alv_properties

  METHOD set_column_styles.
  ENDMETHOD.                    "set_column_styles

  METHOD set_column_text.

    DATA : lv_ltext TYPE scrtext_l,
           lv_mtext TYPE scrtext_m,
           lv_stext TYPE scrtext_s.

    gr_column ?= gr_columns->get_column( i_fname ).
    MOVE : i_text TO lv_ltext.
    gr_column->set_long_text( lv_ltext ).
    MOVE : i_text TO lv_mtext.
    gr_column->set_medium_text( lv_mtext ).
    MOVE : i_text TO lv_stext.
    gr_column->set_short_text( lv_stext  ).

  ENDMETHOD.                    "set_column_text
  METHOD display_alv.
    gr_alv->display( ).
  ENDMETHOD.                    "display_alv
  METHOD on_user_command.

    DATA : lt_rows  TYPE salv_t_row,
           lv_rows  TYPE int4,
           lv_subrc TYPE sy-subrc.

    CASE e_salv_function.
      WHEN '&BATCH'.
        lt_rows = gr_selection->get_selected_rows( ).
        IF lt_rows IS INITIAL.
          MESSAGE 'Lütfen satır seçiniz!!!' TYPE 'S' DISPLAY LIKE 'E'.
          CHECK 1 = 2.
        ENDIF.
        PERFORM batch_input TABLES lt_rows.
        SET USER-COMMAND '&OPT'.
      WHEN '&DELETE'.
        lt_rows = gr_selection->get_selected_rows( ).
        IF lt_rows IS INITIAL.
          MESSAGE 'Lütfen satır seçiniz!!!' TYPE 'S' DISPLAY LIKE 'E'.
          CHECK 1 = 2.
        ENDIF.
        gr_report->delete_rows( EXPORTING lt_rows = lt_rows ).
    ENDCASE.

    gr_alv->refresh( ).
  ENDMETHOD.                    "on_user_command
  METHOD delete_rows .
    DATA: s_rows    TYPE LINE OF salv_t_row.
    DATA: ls_mess   TYPE bapireturn1,
          ls_return TYPE bapireturn.

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

      LOOP AT lt_rows INTO s_rows .
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

*    ---- Personel Lock
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
                operation     = 'DEL'
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
                operation     = 'DEL'
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
                operation     = 'DEL'
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

*    ------ Personel Unlock
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
  ENDMETHOD.
  METHOD get_data_del.
    DATA : lt_9950 TYPE TABLE OF pa9950,
           ls_9950 TYPE pa9950,
           lt_9951 TYPE TABLE OF pa9951,
           ls_9951 TYPE pa9951 .

    IF s_datum-high IS INITIAL .
      s_datum-high = s_datum-low.
    ENDIF.
    CASE 'X'.
      WHEN rb_sabit.
        SELECT * FROM pa9950
          INTO CORRESPONDING FIELDS OF TABLE lt_9950
          WHERE hspsk EQ '1'
            AND begda LE s_datum-high
            AND endda GE s_datum-low .
      WHEN rb_yuzde.
        SELECT * FROM pa9950
          INTO CORRESPONDING FIELDS OF TABLE lt_9950
          WHERE hspsk EQ '4'
            AND begda LE s_datum-high
            AND endda GE s_datum-low  .
      WHEN rb_kesnt.
        SELECT * FROM pa9951
          INTO CORRESPONDING FIELDS OF TABLE lt_9951
            WHERE begda LE s_datum-high
              AND endda GE s_datum-low .
    ENDCASE.

    CASE 'X'.
      WHEN rb_sabit OR rb_yuzde.
        LOOP AT lt_9950 INTO ls_9950.
          CASE 'X'.
            WHEN rb_sabit.
              MOVE-CORRESPONDING ls_9950 TO gs_sabit.
              COLLECT gs_sabit INTO gt_sabit.
            WHEN rb_yuzde.
              MOVE-CORRESPONDING ls_9950 TO gs_yuzde.
              COLLECT gs_yuzde INTO gt_yuzde.
          ENDCASE.
        ENDLOOP.
      WHEN rb_kesnt.
        LOOP AT lt_9951 INTO ls_9951.
          MOVE-CORRESPONDING ls_9950 TO gs_kesnt.
          COLLECT gs_kesnt INTO gt_kesnt.
        ENDLOOP.
      ENDCASE.




*    DATA : lt_pa0015 TYPE TABLE OF pa0015,
*           ls_pa0015 TYPE          pa0015.
*
*    SELECT * FROM pa0015
*      INTO CORRESPONDING FIELDS OF TABLE lt_pa0015
*      WHERE aedtm EQ p_aedtm
*        AND lgart IN s_lgart.
*
*    LOOP AT lt_pa0015 INTO ls_pa0015.
*      gs_report-pernr = ls_pa0015-pernr.
*      gs_report-lgart = ls_pa0015-lgart.
*      gs_report-tutar = ls_pa0015-betrg.
*      gs_report-anzhl = ls_pa0015-anzhl.
*      gs_report-preas = ls_pa0015-preas.
*      gs_report-aedtm = ls_pa0015-aedtm.
*      CONCATENATE ls_pa0015-begda+6(2) ls_pa0015-begda+4(2)
*      ls_pa0015-begda+0(4)
*      INTO gs_report-begda SEPARATED BY '.'.
*      gs_report-endda = ls_pa0015-endda.
*      APPEND gs_report TO gt_report.
*      FREE : gs_report .
*    ENDLOOP.
*

  ENDMETHOD.
  METHOD screen_output.
    " Girilen dosya yolunun çekilmesi
    me->filename_get( ).
*    IF p_file IS INITIAL.
*      MESSAGE text-e02 TYPE 'I'.
*    ENDIF.
  ENDMETHOD.                    "screen_output

  METHOD set_data.




  ENDMETHOD.
  METHOD start_of_selection.
    FIELD-SYMBOLS : <table> TYPE table.
    CASE 'X'.
      WHEN rb_sabit.
        ASSIGN gt_sabit[] TO <table>.
      WHEN rb_yuzde.
        ASSIGN gt_yuzde[] TO <table>.
      WHEN rb_kesnt.
        ASSIGN gt_kesnt[] TO <table>.
    ENDCASE.


    me->get_excel(
         EXPORTING
           file_name = p_file
         CHANGING
           data      = <table>
       ).

    CASE 'X'.
      WHEN rb_sabit.
        MOVE : <table>[] TO gt_sabit[].
      WHEN rb_yuzde.
        MOVE : <table>[] TO gt_yuzde[].
      WHEN rb_kesnt.
        MOVE : <table>[] TO gt_kesnt[].
    ENDCASE.
  ENDMETHOD.

  METHOD get_excel.

    DATA : l_file_name TYPE ibipparms-path.
    DATA : filename  TYPE  rlgrap-filename.

    FIELD-SYMBOLS: <fs> TYPE any.
    DATA: dref1 TYPE REF TO data.
    DATA: dref2 TYPE REF TO data.
    FIELD-SYMBOLS <table> LIKE data.
    FIELD-SYMBOLS <wa>    TYPE any.

    DATA : ld_index     TYPE sy-index.
    DATA : lv_fname     TYPE rlgrap-filename.
    DATA : p_scol  TYPE i VALUE '1',
           p_srow  TYPE i VALUE '1',
           p_ecol  TYPE i VALUE '256',
           p_erow  TYPE i VALUE '65536',
           lv_type TYPE c.
    DATA : lv_string1 TYPE string,
           lv_string2 TYPE string,
           lv_string3 TYPE string.
    DATA : lv_length TYPE i.

    DATA : lv_decimal TYPE p LENGTH 10 DECIMALS 2.

    DATA : lt_intern TYPE  TABLE OF kcde_cells .


    IF file_name IS INITIAL .
      CALL FUNCTION 'F4_FILENAME'
        IMPORTING
          file_name = l_file_name.

      filename = l_file_name.
    ELSE.
      filename = file_name.
    ENDIF.

    CREATE DATA dref1 LIKE data.
    ASSIGN dref1->* TO <table>.

    CREATE DATA dref2 LIKE LINE OF <table>.
    ASSIGN dref2->* TO <wa>.

* Note: Alternative function module - 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
      EXPORTING
        filename                = filename
        i_begin_col             = p_scol
        i_begin_row             = p_srow
        i_end_col               = p_ecol
        i_end_row               = p_erow
      TABLES
        intern                  = lt_intern
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      FORMAT COLOR COL_BACKGROUND INTENSIFIED.
      WRITE:/ 'Dosya Yüklenirken Hata !'.
      EXIT.
    ENDIF.
    IF lt_intern[] IS INITIAL.
      CHECK 1 EQ 2.
    ELSE.
      SORT lt_intern BY row col.
      DELETE lt_intern WHERE row EQ 1.
    ENDIF.

    LOOP AT lt_intern ASSIGNING FIELD-SYMBOL(<f1>).
      MOVE : <f1>-col TO ld_index.
      UNASSIGN <fs> .
*      DO 1000 TIMES.
        ASSIGN COMPONENT ld_index OF STRUCTURE <wa> TO <fs> .
*        ASSIGN COMPONENT <f1>-col OF STRUCTURE <wa> TO <fs> .
*        IF <fs> IS ASSIGNED .
*          EXIT.
*        ENDIF.
*      ENDDO.
      DESCRIBE FIELD <fs> TYPE lv_type.
      CASE lv_type.
        WHEN 'D'.

          IF <f1>-value IS NOT INITIAL OR
             <f1>-value+0(8) NE '0000000'.


            SPLIT <f1>-value AT '.' INTO lv_string1 lv_string2
            lv_string3.

            lv_length = strlen( lv_string1 ).
            IF lv_length EQ 1.

              CONCATENATE '0' lv_string1  INTO lv_string1.

            ENDIF.
            CLEAR lv_length.

            lv_length = strlen( lv_string2 ).
            IF lv_length EQ 1.

              CONCATENATE '0' lv_string2  INTO lv_string2.

            ENDIF.

            CONCATENATE lv_string1 lv_string2 lv_string3 INTO <f1>-value
            .


            CONCATENATE <f1>-value+4(4)   <f1>-value+2(2)
                         <f1>-value+0(2) INTO <fs>.
          ENDIF.
        WHEN 'P'.

          CALL FUNCTION 'MOVE_CHAR_TO_NUM'
            EXPORTING
              chr             = <f1>-value
            IMPORTING
              num             = lv_decimal
            EXCEPTIONS
              convt_no_number = 1
              convt_overflow  = 2
              OTHERS          = 3.
          MOVE lv_decimal TO <fs>.

        WHEN OTHERS.
          CALL FUNCTION 'ZHRIGA_FG036_003'
            EXPORTING
              i_value     = <f1>-value
              i_variable  = ','
              i_separator = '.'
            IMPORTING
              e_value     = <f1>-value
            EXCEPTIONS
              not_in      = 1
              OTHERS      = 2.

          MOVE <f1>-value TO <fs>.

      ENDCASE.

      AT END OF row.
        APPEND <wa> TO data.
        CLEAR <wa>.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
