*&---------------------------------------------------------------------*
*&  Include           ZHRBR_P003_I005
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  OPEN_EXCEL
*&---------------------------------------------------------------------*
FORM open_excel .
  DATA: l_cnt TYPE i.
*
* create object excel of OLE class 'EXCEL.APPLICATION'
*
  CREATE OBJECT g_excel 'EXCEL.APPLICATION'.
  CALL METHOD OF
      g_excel
      'WORKBOOKS' = g_workbooks.
  CALL METHOD OF
      g_workbooks
      'ADD'       = g_workbook.
  GET PROPERTY OF g_workbook 'Worksheets' = g_worksheets.
  GET PROPERTY OF g_excel 'ACTIVESHEET' = g_worksheet.
  SET PROPERTY OF g_excel 'DISPLAYALERTS' = 0.

  GET PROPERTY OF g_worksheets 'Count' = l_cnt.

  l_cnt = l_cnt - 1.
* Delete unwanted worksheets
  DO l_cnt TIMES.
    GET PROPERTY OF g_excel 'ACTIVESHEET' = g_worksheet.
    CALL METHOD OF
        g_worksheet
        'DELETE'.
  ENDDO.

ENDFORM.                    " OPEN_EXCEL
*&---------------------------------------------------------------------*
*&      Form  ADD_WORKSHEET1
*&---------------------------------------------------------------------*
FORM add_worksheet1 .

  PERFORM add_worksheet USING 'Sayfa 1'.
  PERFORM write_items.
  PERFORM write_header.

ENDFORM.                    " ADD_WORKSHEET1
*&---------------------------------------------------------------------*
*&      Form  ADD_WORKSHEET
*&---------------------------------------------------------------------*
FORM add_worksheet USING i_name.
* Add new worksheet
  IF g_first_ws <> 'N'.
    g_first_ws = 'N'.
    GET PROPERTY OF g_excel 'ACTIVESHEET' = g_worksheet.
  ELSE.
    CALL METHOD OF
        g_worksheets
        'Add'        = g_worksheet.
  ENDIF.
  SET PROPERTY OF g_worksheet 'NAME' = i_name.
  g_row = 1.
  g_col = 1.
ENDFORM. " ADD_WORKSHEET
*&---------------------------------------------------------------------*
*& Form FILL_CELL
*& FORM routine, which fills the specified cell in the EXCEL sheet
*& with the given value
*&---------------------------------------------------------------------*
FORM fill_cell USING i_row TYPE i
i_col TYPE i
i_value
i_fontbold
i_digit
i_wraptext
i_horizon_align
i_vertical_align.

  DATA: l_str TYPE string.

  CALL METHOD OF
      g_excel
      'CELLS' = g_cell
    EXPORTING
      #1      = i_row
      #2      = i_col.

  SET PROPERTY OF g_cell 'VALUE' = i_value.

  IF i_fontbold = 'X'.
    GET PROPERTY OF g_cell 'Font' = g_font.
    SET PROPERTY OF g_font 'Bold' = 1.
  ENDIF.

  IF NOT i_wraptext IS INITIAL.
    SET PROPERTY OF g_cell 'WrapText' = 1.
  ENDIF.

  IF NOT i_horizon_align IS INITIAL.
    IF i_horizon_align = 'L'.
      SET PROPERTY OF g_cell 'HorizontalAlignment' = xlleft.
    ELSEIF i_horizon_align = 'R'.
      SET PROPERTY OF g_cell 'HorizontalAlignment' = xlright.
    ELSEIF i_horizon_align = 'C'.
      SET PROPERTY OF g_cell 'HorizontalAlignment' = xlcenter.
    ENDIF.
  ENDIF.

  IF NOT i_vertical_align IS INITIAL.
    IF i_vertical_align = 'T'.
      SET PROPERTY OF g_cell 'VerticalAlignment' = xltop.
    ELSEIF i_vertical_align = 'B'.
      SET PROPERTY OF g_cell 'VerticalAlignment' = xlbottom.
    ELSEIF i_vertical_align = 'C'.
      SET PROPERTY OF g_cell 'VerticalAlignment' = xlcenter.
    ENDIF.
  ENDIF.

* To set number format for cell
  IF i_digit <> ''.
    IF i_value IS INITIAL AND i_digit <> '%'.
      SET PROPERTY OF g_cell 'VALUE' = ''.
    ELSE.
* Set number format for cell with number
      IF i_digit = '1'.
        SET PROPERTY OF g_cell 'NumberFormat' = '#,###.0 '.
      ELSEIF i_digit = '2'.
        SET PROPERTY OF g_cell 'NumberFormat' = '#,##0.00 '.
      ELSEIF i_digit = '%'.
        SET PROPERTY OF g_cell 'NumberFormat' = '#,##0.00% '.
      ELSE.
        SET PROPERTY OF g_cell 'NumberFormat' = '#,### '.
      ENDIF.
    ENDIF.
  ENDIF.
  ADD 1 TO i_col.
ENDFORM.                    "FILL_CELL
*&---------------------------------------------------------------------*
*& Form MERGE_CELL
*&---------------------------------------------------------------------*
FORM merge_cell USING i_row1 i_col1
i_row2 i_col2.
  CALL METHOD OF
      g_excel
      'Cells' = g_cell1
    EXPORTING
      #1      = i_row1
      #2      = i_col1.

  CALL METHOD OF
      g_excel
      'Cells' = g_cell2
    EXPORTING
      #1      = i_row2
      #2      = i_col2.

  CALL METHOD OF
      g_excel
      'Range' = g_cellrange
    EXPORTING
      #1      = g_cell1
      #2      = g_cell2.

  CALL METHOD OF
      g_cellrange
      'Merge'.
ENDFORM. " MERGE_CELL
*&---------------------------------------------------------------------*
*& Form SET_BORDER
*&---------------------------------------------------------------------*
FORM set_border USING i_row1 i_col1
i_row2 i_col2
i_thickness
i_allborders.

  CALL METHOD OF
      g_excel
      'Cells' = g_cell1
    EXPORTING
      #1      = i_row1
      #2      = i_col1.

  CALL METHOD OF
      g_excel
      'Cells' = g_cell2
    EXPORTING
      #1      = i_row2
      #2      = i_col2.

  CALL METHOD OF
      g_excel
      'Range' = g_cellrange
    EXPORTING
      #1      = g_cell1
      #2      = g_cell2.

  IF i_allborders IS INITIAL.
    CALL METHOD OF
        g_cellrange
        'BorderAround'

      EXPORTING
        #1             = 1 "Continuous line
        #2             = i_thickness. "Thickness: 1 - Normal, 4 - Thick
  ELSE.
    GET PROPERTY OF g_cellrange 'Borders' = g_borders.
    SET PROPERTY OF g_borders 'LineStyle' = '1'.
    SET PROPERTY OF g_borders 'Weight' = i_thickness.
  ENDIF.
ENDFORM. " SET_BORDER
*&---------------------------------------------------------------------*
*& Form SET_COLOR
*&---------------------------------------------------------------------*
FORM set_color USING i_row1 i_col1
i_row2 i_col2
i_color.

  DATA: l_colorindex TYPE i.

  CASE i_color.
    WHEN 'BK'.
      l_colorindex = 1. "Black
    WHEN 'BR'.
      l_colorindex = 53. "Brown
    WHEN 'OG'.
      l_colorindex = 52. "Olive Green
    WHEN 'DG'.
      l_colorindex = 51. "Dark Green
    WHEN 'DT'.
      l_colorindex = 49. "Dark Teal
    WHEN 'DB'.
      l_colorindex = 11. "Dark Blue
    WHEN 'ID'.
      l_colorindex = 55. "Indigo
    WHEN 'G4'.
      l_colorindex = 56.                                    "Gray 80%
    WHEN 'DR'.
      l_colorindex = 9. "Dark Red
    WHEN 'OR'.
      l_colorindex = 46. "Orange
    WHEN 'DY'.
      l_colorindex = 12. "Dark Yellow
    WHEN 'GR'.
      l_colorindex = 10. "Green
    WHEN 'TL'.
      l_colorindex = 14. "Teal
    WHEN 'BL'.
      l_colorindex = 5. "Blue
    WHEN 'BY'.
      l_colorindex = 47. "Blue Gray
    WHEN 'G3'.
      l_colorindex = 16.                                    "Gray 50%
    WHEN 'RD'.
      l_colorindex = 3. "Red
    WHEN 'LO'.
      l_colorindex = 45. "Light Orange
    WHEN 'LI'.
      l_colorindex = 43. "Lime
    WHEN 'SG'.
      l_colorindex = 50. "Sea Green
    WHEN 'AQ'.
      l_colorindex = 42. "Aqua
    WHEN 'LB'.
      l_colorindex = 41. "Light Blue
    WHEN 'VL'.
      l_colorindex = 13. "Violet
    WHEN 'G2'.
      l_colorindex = 48.                                    "Gray 40%
    WHEN 'PK'.
      l_colorindex = 7. "Pink
    WHEN 'GD'.
      l_colorindex = 44. "Gold
    WHEN 'YL'.
      l_colorindex = 6. "Yellow
    WHEN 'BG'.
      l_colorindex = 4. "Bright Green
    WHEN 'TQ'.
      l_colorindex = 8. "Turquoise
    WHEN 'SB'.
      l_colorindex = 33. "Sky Blue
    WHEN 'PL'.
      l_colorindex = 54. "Plum
    WHEN 'G1'.
      l_colorindex = 15.                                    "Gray 25%
    WHEN 'RS'.
      l_colorindex = 38. "Rose
    WHEN 'TN'.
      l_colorindex = 40. "Tan
    WHEN 'LY'.
      l_colorindex = 36. "Light Yellow
    WHEN 'LG'.
      l_colorindex = 35. "Light Green
    WHEN 'LT'.
      l_colorindex = 34. "Light Turquoise
    WHEN 'PB'.
      l_colorindex = 37. "Pale Blue
    WHEN 'LV'.
      l_colorindex = 39. "Lavender
    WHEN 'WH'.
      l_colorindex = 2. "White
    WHEN OTHERS.
      l_colorindex = 2. "White
  ENDCASE.

  CALL METHOD OF
      g_excel
      'Cells' = g_cell1
    EXPORTING
      #1      = i_row1
      #2      = i_col1.

  CALL METHOD OF
      g_excel
      'Cells' = g_cell2
    EXPORTING
      #1      = i_row2
      #2      = i_col2.

  CALL METHOD OF
      g_excel
      'Range' = g_cellrange
    EXPORTING
      #1      = g_cell1
      #2      = g_cell2.

  GET PROPERTY OF g_cellrange 'Interior' = g_interior.

  SET PROPERTY OF g_interior 'ColorIndex' = l_colorindex.
ENDFORM. " SET_COLOR
*&---------------------------------------------------------------------*
*& Form COPY_CELL
*&---------------------------------------------------------------------*
FORM copy_cell USING i_crow i_ccol
i_prow1 i_pcol1
i_prow2 i_pcol2.
  CALL METHOD OF
      g_excel
      'Cells' = g_cell
    EXPORTING
      #1      = i_crow
      #2      = i_ccol.

  CALL METHOD OF
      g_cell
      'Copy'.

  CALL METHOD OF
      g_excel
      'Cells' = g_cell1
    EXPORTING
      #1      = i_prow1
      #2      = i_pcol1.

  CALL METHOD OF
      g_excel
      'Cells' = g_cell2
    EXPORTING
      #1      = i_prow2
      #2      = i_pcol2.

  CALL METHOD OF
      g_excel
      'Range' = g_cellrange
    EXPORTING
      #1      = g_cell1
      #2      = g_cell2.

  CALL METHOD OF
      g_worksheet
      'Paste'

    EXPORTING
      #1          = g_cellrange.

ENDFORM. " COPY_CELL
*&---------------------------------------------------------------------*
*& Form OPEN_TEMPLATE_FILE
*&---------------------------------------------------------------------*
FORM open_template_file.
  CALL METHOD OF
      g_workbooks
      'Open'      = g_tmp_workbook
    EXPORTING
      #1          = g_pc_template
      #2          = 2
      #3          = 0
      #4          = 1
      #5          = 0
      #6          = 0
      #7          = 1.

  CALL FUNCTION 'FLUSH'
    EXCEPTIONS
      OTHERS = 0.

  GET PROPERTY OF g_tmp_workbook 'Worksheets' = g_tmp_worksheets.
  GET PROPERTY OF g_tmp_worksheets 'Item' = g_tmp_worksheet1
  EXPORTING #1 = 1.
  GET PROPERTY OF g_tmp_worksheets 'Item' = g_tmp_worksheet2
  EXPORTING #1 = 2.

  CALL METHOD OF
      g_tmp_worksheet1
      'Copy'

    EXPORTING
      #1               = g_worksheet.

  CALL METHOD OF
      g_tmp_worksheet2
      'Copy'

    EXPORTING
      #1               = g_worksheet.

* call method of g_tmp_workbook 'Close'.
  CALL FUNCTION 'FLUSH'
    EXCEPTIONS
      OTHERS = 0.

ENDFORM. " OPEN_TEMPLATE_FILE
*&---------------------------------------------------------------------*
*& Form CLOSE_EXCEL
*&---------------------------------------------------------------------*
FORM close_excel.

  DATA : lf_file       TYPE string,
       lf_filename   TYPE string,
       lf_path       TYPE string,
       lf_fullpath   TYPE string,
       lf_usr_action TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = 'Sayfa1'
*     initial_directory    = 'C:\'
      default_file_name    = 'ICRA_TALIMAT_CIKTISI.xlsx'
      default_extension    = 'xlsx'
      file_filter          = '*.xlsx'
    CHANGING
      filename             = lf_filename
      path                 = lf_path
      fullpath             = lf_fullpath
      user_action          = lf_usr_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc EQ 0 and lf_usr_action eq 0.
    CALL METHOD OF
        g_excel
        'Columns' = g_column.
    CALL METHOD OF
        g_column
        'Autofit'.

    SET PROPERTY OF g_column 'ColumnWidth' = 21.


    SET PROPERTY OF g_excel 'VISIBLE' = 1.

    CALL METHOD OF
        g_workbook
        'SAVEAS'

      EXPORTING
        #1         = lf_fullpath.

  ENDIF.
*
* quit Excel and free all OLE objects
*
* call method of g_excel 'QUIT'.
  FREE OBJECT g_interior.
  FREE OBJECT g_borders.
  FREE OBJECT g_font.
  FREE OBJECT g_cell.
  FREE OBJECT g_cell1.
  FREE OBJECT g_cell2.
  FREE OBJECT g_cellrange.
  FREE OBJECT g_excel.
  FREE OBJECT g_column.
  FREE OBJECT g_workbooks.
  FREE OBJECT g_workbook.
  FREE OBJECT g_worksheets.
  FREE OBJECT g_worksheet.

ENDFORM. " CLOSE_EXCEL
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&---------------------------------------------------------------------*
FORM write_header .
  DATA: lv_datum(10).
* fill line in EXCEL sheet with headerlines of table columns
  WRITE sy-datum TO lv_datum USING EDIT MASK '__.__.____'.
  g_col = 7.
  PERFORM fill_cell USING 1 g_col lv_datum 'X' '' '' '' ''.

  DATA: lv_str TYPE string.
  CONCATENATE ' T. VAKIFLAR BANKASI T.A.O.'
               gc_newline
               p_icram
               gc_newline
               gc_newline
               gc_newline
               p_bankn
               text-h01
               gc_newline
               gc_newline
               gc_newline
          INTO lv_str SEPARATED BY space.

  DO 70 TIMES.
    CONCATENATE lv_str gc_space
       INTO lv_str SEPARATED BY space.
  ENDDO.
  CONCATENATE lv_str 'HASÇELİK KABLO SAN. TİC. A.Ş.'
    INTO lv_str SEPARATED BY space.

  g_col = 2.
  PERFORM merge_cell USING 2 2 11 7.
  PERFORM fill_cell USING 2 g_col lv_str '' ''
          'X' 'L' 'T'.

ENDFORM.                    " WRITE_HEADER
*&---------------------------------------------------------------------*
*&      Form  WRITE_ITEMS
*&---------------------------------------------------------------------*
FORM write_items .
  DATA: lv_total(30),
        lv_row(3),
        lv_sira TYPE i.

  g_row = 14.
  g_col = 1.

  PERFORM fill_cell USING g_row g_col 'Sıra No'        'X' '' '' '' ''.
  PERFORM fill_cell USING g_row g_col 'İCRA MÜDÜRLÜĞÜ' 'X' '' '' '' ''.
  PERFORM fill_cell USING g_row g_col 'İCRA DAİRESİ IBAN' 'X' '' '' '' ''.
  PERFORM fill_cell USING g_row g_col 'Dosya No'       'X' '' '' '' ''.
  PERFORM fill_cell USING g_row g_col 'Adı SOYADI'     'X' '' '' '' ''.
  PERFORM fill_cell USING g_row g_col 'Tutar'          'X' '' '' '' ''.
  PERFORM fill_cell USING g_row g_col 'Açıklama'       'X' '' '' '' ''.
  PERFORM fill_cell USING g_row g_col 'Şirket'         'X' '' '' '' ''.

*  PERFORM set_color USING g_row 1 g_row g_col 'LT'.

  ADD 1 TO g_row.
  CLEAR: lv_sira.
  lv_sira = 1.
  LOOP AT gt_report INTO gs_report.
    g_col = 1.
    PERFORM fill_cell USING g_row g_col lv_sira '' '' ''   '' ''.
    PERFORM fill_cell USING g_row g_col gs_report-icraa ''  '' '' '' ''.
    PERFORM fill_cell USING g_row g_col gs_report-icaib ''  '' '' '' ''.
    PERFORM fill_cell USING g_row g_col gs_report-dosno ''  '' '' '' ''.
    PERFORM fill_cell USING g_row g_col gs_report-ename ''  '' '' '' ''.
    PERFORM fill_cell USING g_row g_col gs_report-odmtr ''  '2' '' '' ''.
    PERFORM fill_cell USING g_row g_col p_acklm         ''  '' '' '' ''.
    PERFORM fill_cell USING g_row g_col gs_report-butxt ''  '' '' '' ''.
    IF gs_report-stext IS NOT INITIAL.
      PERFORM fill_cell USING g_row g_col gs_report-stext ''  '' '' '' ''.
    ENDIF.
    ADD 1 TO g_row.
    ADD 1 TO lv_sira.
  ENDLOOP.
  WRITE g_row TO lv_row USING EDIT MASK '___'.
  lv_row = lv_row - '001'.
  SHIFT lv_row LEFT DELETING LEADING '0'.
  g_col = 5.
  PERFORM fill_cell USING g_row g_col 'Toplam'          'X' '' '' '' ''.
  lv_total = '=SUM(E15:' && 'F' && lv_row && ')'.
  PERFORM fill_cell USING g_row g_col lv_total          'X' '2' '' '' ''.


ENDFORM.                    " WRITE_ITEMS
