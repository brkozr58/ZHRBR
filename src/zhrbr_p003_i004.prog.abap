*&---------------------------------------------------------------------*
*&  Include           ZHRBR_P003_I004
*&---------------------------------------------------------------------*
INITIALIZATION.
  CREATE OBJECT gr_report.
  gr_report->set_init( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_spmon.
  gr_report->set_period( ).

START-OF-SELECTION.

  gr_report->set_date( ).

GET pernr.

  rp_provide_from_last p0000 space pn-begda pn-endda.
  rp_provide_from_last p0001 space pn-begda pn-endda.
  rp-set-data-interval 'P9950' '18000101' '99991231'.
  rp-set-data-interval 'P9951' '18000101' '99991231'.

  gr_report->get_person( ).

END-OF-SELECTION.

  SORT gt_report ASCENDING BY pernr.
  IF gt_report[] IS NOT INITIAL.
    gr_report->create_excel( ).
  ELSE.
    MESSAGE s001(00) WITH 'Şeçim kriterlerine göre veri bulunamadı'
    DISPLAY LIKE 'E'.
  ENDIF.
