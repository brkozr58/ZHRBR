*&---------------------------------------------------------------------*
*&  Include           ZHRBR_P003_I003
*&---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.
    METHODS :
      set_init,
      set_date,
      set_period,
      hr_get_subtype_text
        IMPORTING i_subty TYPE subty
        CHANGING  c_stext TYPE sbttx,
      get_person,
      create_excel.
*      prepare_alv,
*      pdf IMPORTING rows TYPE salv_t_row,
*      display_alv.

  PROTECTED SECTION.
*    DATA : gr_alv       TYPE REF TO cl_salv_table,
*           gr_past      TYPE REF TO cl_salv_table,
*           gr_display   TYPE REF TO cl_salv_display_settings,
*           gr_columns   TYPE REF TO cl_salv_columns_table,
*           gr_column    TYPE REF TO cl_salv_column_table,
*           gr_functions TYPE REF TO cl_salv_functions_list,
*           gr_selection TYPE REF TO cl_salv_selections,
*           gr_layout    TYPE REF TO cl_salv_layout,
*           gr_events    TYPE REF TO cl_salv_events_table,
*           gr_exp_msg   TYPE REF TO cx_salv_msg.
*
*    DATA : gs_key   TYPE salv_s_layout_key.

  PRIVATE SECTION.
*    METHODS :
*      create_alv,
*      set_pf_status,
*      set_top_of_page,
*      set_alv_properties,
*      set_column_styles,
*      set_column_text
*        IMPORTING i_fname TYPE lvc_fname
*                  i_text  TYPE any,
*      on_user_command FOR EVENT added_function OF cl_salv_events
*        IMPORTING e_salv_function.

ENDCLASS.                    "lcl_report DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.
  METHOD set_init.

    SELECT * FROM t001
             INTO TABLE gt_t001
             WHERE land1 EQ 'TR'
               AND spras EQ 'T'.

    SELECT * FROM zhrbr_t006
             INTO TABLE gt_t006.

  ENDMETHOD.                    "set_init
  METHOD set_date.
    CLEAR: gv_begda , gv_endda.

    gv_begda = p_spmon && '01'.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = gv_begda
      IMPORTING
        last_day_of_month = gv_endda.
*
    pn-begps = pnpbegps = pn-begda = pnpbegda = gv_begda  .
    pn-endps = pnpendps = pn-endda = pnpendda = gv_endda .
  ENDMETHOD.                    "set_date
  METHOD set_period.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        actual_month               = sy-datum(6)
      IMPORTING
        selected_month             = p_spmon
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        month_not_found            = 3
        OTHERS                     = 4.

  ENDMETHOD.                    "set_date
*  METHOD prepare_alv.
*    me->create_alv( ).
*    me->set_pf_status( ).
*    me->set_alv_properties( ).
*    me->set_top_of_page( ).
*    me->display_alv( ).
*  ENDMETHOD.                    "prepare_alv

*  METHOD create_alv.
*    TRY.
*        cl_salv_table=>factory(
*                  IMPORTING
*                    r_salv_table = gr_alv
*                  CHANGING
*                    t_table      = gt_report ).
*      CATCH
*        cx_salv_msg INTO gr_exp_msg.
*    ENDTRY.
*
*  ENDMETHOD.                    "create_alv

*  METHOD set_pf_status.
*    gr_alv->set_screen_status(
*                pfstatus      = 'GUI'
*                report        = sy-repid
*                set_functions = gr_alv->c_functions_all ).
*  ENDMETHOD.                    "set_pf_status

*  METHOD set_top_of_page.
*
*    DATA : lo_header      TYPE REF TO cl_salv_form_layout_grid,
*           lo_grid_bottom TYPE REF TO cl_salv_form_layout_grid,
*           lo_logo        TYPE REF TO cl_salv_form_layout_logo,
*           lo_text        TYPE REF TO cl_salv_form_text,
*           lo_label       TYPE REF TO cl_salv_form_label.
*
*    DATA : lv_date  TYPE text10.
*
*    CREATE OBJECT lo_header.
*
*    lo_header->create_header_information(
*                    row    = 1
*                    column = 1
*                    text   = text-t01 )  . " Rapor ismi
*    lo_header->add_row( ).
*
*    lo_grid_bottom = lo_header->create_grid(
*                   row    = 3
*                   column = 1 ).
*
*    lo_label = lo_grid_bottom->create_label(
*                   row     = 1
*                   column  = 1
*                   text    = text-t02
*                   tooltip = text-t02 ). " Tarih
*
*    WRITE sy-datum TO lv_date DD/MM/YYYY.
*    lo_text = lo_grid_bottom->create_text(
*                   row     = 1
*                   column  = 2
*                   text    = lv_date
*                   tooltip = lv_date ). " Tarih
*
*    lo_label = lo_grid_bottom->create_label(
*                   row     = 2
*                   column  = 1
*                   text    = text-t03
*                   tooltip = text-t03 ). " Kullanıcı
*
*    lo_text = lo_grid_bottom->create_text(
*                   row     = 2
*                   column  = 2
*                   text    = sy-uname
*                   tooltip = sy-uname ). " Kullanıcı
*
*
*    lo_label->set_label_for( lo_text ).
*
*
*  ENDMETHOD.                    "set_top_of_page

*  METHOD set_alv_properties.
*
*    gr_display = gr_alv->get_display_settings( ).
*
** Zebra sytle..
*    gr_display->set_striped_pattern( cl_salv_display_settings=>true ).
*
*    gr_columns = gr_alv->get_columns( ).
** Set optimize..
*    gr_columns->set_optimize( abap_true ).
*
*    gr_layout = gr_alv->get_layout( ).
** Set variant..
*    gs_key-report = sy-repid.
*    gr_layout->set_key( gs_key ).
*    gr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
*
** Set selection..
*    gr_selection = gr_alv->get_selections( ).
*    gr_selection->set_selection_mode( if_salv_c_selection_mode=>cell ).
*
*    gr_events = gr_alv->get_event( ).
** Set ALV Events.
*    SET HANDLER gr_report->on_user_command FOR gr_events.
*
** Set Column Texts.
*    me->set_column_text( i_fname = 'PERNR'  i_text = text-001 ).
*    me->set_column_text( i_fname = 'ENAME'  i_text = text-002 ).
*    me->set_column_text( i_fname = 'DOSNO'  i_text = text-003 ).
*    me->set_column_text( i_fname = 'ICRAA'  i_text = text-004 ).
*    me->set_column_text( i_fname = 'ALICI'  i_text = text-005 ).
*    me->set_column_text( i_fname = 'AIBAN'  i_text = text-006 ).
*    me->set_column_text( i_fname = 'ABORC'  i_text = text-007 ).
*    me->set_column_text( i_fname = 'DONEM'  i_text = text-008 ).
*    me->set_column_text( i_fname = 'ODMTR'  i_text = text-009 ).
*    me->set_column_text( i_fname = 'TKEST'  i_text = text-010 ).
*    me->set_column_text( i_fname = 'BBORC'  i_text = text-011 ).
*
*
*
*    me->set_column_styles( ).
**    HIDE columns.
*    gr_column ?= gr_columns->get_column( 'ICRAD' ).
*    gr_column->set_technical(  if_salv_c_bool_sap=>true ).
**    HIDE columns.
*    gr_column ?= gr_columns->get_column( 'SUBTX' ).
*    gr_column->set_technical(  if_salv_c_bool_sap=>true ).
*
*
*  ENDMETHOD.                    "set_alv_properties

*  METHOD set_column_styles.
*  ENDMETHOD.                    "set_column_styles

*  METHOD set_column_text.
*
*    DATA : lv_ltext TYPE scrtext_l,
*           lv_mtext TYPE scrtext_m,
*           lv_stext TYPE scrtext_s.
*
*    gr_column ?= gr_columns->get_column( i_fname ).
*    MOVE : i_text TO lv_ltext.
*    gr_column->set_long_text( lv_ltext ).
*    MOVE : i_text TO lv_mtext.
*    gr_column->set_medium_text( lv_mtext ).
*    MOVE : i_text TO lv_stext.
*    gr_column->set_short_text( lv_stext  ).
*
*  ENDMETHOD.                    "set_column_text
*  METHOD display_alv.
*    gr_alv->display( ).
*  ENDMETHOD.                    "display_alv
  METHOD get_person.
    DATA : s9951    TYPE p9951.
    DATA : ls_p9950 TYPE p9950.
    DATA : ls_t006  TYPE zhrbr_t006.
    CHECK pernr-pernr IN pnppernr AND
          pernr-bukrs IN pnpbukrs AND
          pernr-werks IN pnpwerks AND
          pernr-btrtl IN pnpbtrtl AND
          pernr-persg IN pnppersg AND
          pernr-persk IN pnppersk AND
          pernr-kostl IN pnpkostl AND
          pernr-plans IN pnpplans AND
          pernr-orgeh IN pnporgeh AND
          pernr-stell IN pnpstell AND
          pernr-abkrs IN pnpabkrs AND
          pernr-ansvh IN pnpansvh AND
          pernr-vdsk1 IN pnpvdsk1.
    CHECK p9951[] IS NOT INITIAL.
    LOOP AT  p9951 INTO s9951 WHERE begda LE gv_endda
                               AND  endda GE gv_begda.
      gs_report-pernr = p0001-pernr.
      gs_report-ename = p0001-ename.
      gs_report-odmtr = s9951-odmtr.
      gs_report-icrid = s9951-icrid.
      READ TABLE gt_t001 INTO gs_t001 WITH  KEY bukrs = p0001-bukrs.
      IF sy-subrc EQ 0.
        gs_report-butxt = gs_t001-butxt."şirket texti
      ENDIF.

      READ TABLE p9950  INTO ls_p9950
                        WITH KEY icrid = s9951-icrid.
      IF sy-subrc EQ 0.
        gs_report-icrad = ls_p9950-icrad.
        READ TABLE gt_t006 INTO ls_t006
                           WITH KEY icrad = ls_p9950-icrad.
        IF sy-subrc EQ 0.
          gs_report-icraa = ls_t006-icraa.
          gs_report-icaib = ls_t006-icaib.
        ENDIF.
        IF ls_p9950-subty EQ '2550'.
          gs_report-stext = 'NAFAKA'.
        ENDIF.
        gs_report-dosno = ls_p9950-dosno.
      ENDIF.

      APPEND: gs_report TO  gt_report.
      CLEAR: gs_report,ls_t006,ls_p9950,gs_t001.
    ENDLOOP.
  ENDMETHOD.                    "get_person
  METHOD create_excel.
    PERFORM open_excel.
    PERFORM add_worksheet1.
    PERFORM open_template_file.
    PERFORM close_excel.
  ENDMETHOD.                    " create_excel
  METHOD hr_get_subtype_text.

    CALL FUNCTION 'HR_GET_SUBTYPE_TEXT'
      EXPORTING
        infty               = '9950'
        subty               = i_subty
      IMPORTING
        stext               = c_stext
      EXCEPTIONS
        infty_not_found     = 1
        subty_not_found     = 2
        infty_not_supported = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.                    "hr_get_subtype_text
*  METHOD on_user_command.
*    DATA : lt_rows  TYPE salv_t_row,
*           lv_rows  TYPE int4,
*           lv_subrc TYPE sy-subrc.
**
*    CASE e_salv_function.
*      WHEN '&F03' OR '&F15' OR '&F12'.
*        CALL  SCREEN 0.
**      WHEN '&PDF'.
**        lt_rows = gr_selection->get_selected_rows( ).
**        DESCRIBE TABLE lt_rows.
**        IF sy-tfill < 1.
***          MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'E'.
**        ELSE.
**          me->pdf( rows = lt_rows ).
**        ENDIF.
*    ENDCASE.
**
*    gr_alv->refresh( ).
*
*  ENDMETHOD.                    "on_user_command

*  METHOD pdf.
*    DATA : fp_outputparams TYPE sfpoutputparams,
*           fp_docparams    TYPE sfpdocparams,
*           fp_formoutput   TYPE fpformoutput,
*           fp_result       TYPE sfpjoboutput,
*           lt_report       TYPE TABLE OF zetpa_s019,
*           ls_report       TYPE zetpa_s019,
*           lv_fm_name      TYPE rs38l_fnam.
*
*    CLEAR lt_report[].
*    LOOP AT rows INTO DATA(ls_row).
*      READ TABLE gt_report INTO DATA(ok_report) INDEX ls_row.
*      MOVE-CORRESPONDING ok_report TO ls_report.
*      APPEND ls_report TO lt_report.
*    ENDLOOP.
*    CHECK lt_report[] IS NOT INITIAL.
*    fp_docparams-langu    = sy-langu.
*    fp_docparams-country  = 'TR'.
*
**    fp_outputparams-nodialog = 'X'.
**    fp_outputparams-getpdf   = 'X'.
*
*    CALL FUNCTION 'FP_JOB_OPEN'
*      CHANGING
*        ie_outputparams = fp_outputparams
*      EXCEPTIONS
*        cancel          = 1
*        usage_error     = 2
*        system_error    = 3
*        internal_error  = 4
*        OTHERS          = 5.
*    CHECK sy-subrc = 0.
*
*    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
*      EXPORTING
*        i_name     = 'ZETPA_AF002'
*      IMPORTING
*        e_funcname = lv_fm_name.
*
*    IF lv_fm_name IS NOT INITIAL .
*      CALL FUNCTION lv_fm_name "'/1BCDWB/SM00000005'
*        EXPORTING
*          /1bcdwb/docparams  = fp_docparams
*          gt_data            = lt_report
*        IMPORTING
*          /1bcdwb/formoutput = fp_formoutput
*        EXCEPTIONS
*          usage_error        = 1
*          system_error       = 2
*          internal_error     = 3
*          OTHERS             = 4.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*    ENDIF .
*
*    CALL FUNCTION 'FP_JOB_CLOSE'
*      IMPORTING
*        e_result       = fp_result
*      EXCEPTIONS
*        usage_error    = 1
*        system_error   = 2
*        internal_error = 3
*        OTHERS         = 4.
*

*  ENDMETHOD.                    "pdf
ENDCLASS. "lcl_report IMPLEMENTATION
