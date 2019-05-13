*&---------------------------------------------------------------------*
*&  Include           Z_OOP_MVC_1_C01
*&---------------------------------------------------------------------*

*
* Класс-сущность предоставляющия доступ к селекционному экрану
* в программе прямой доступ к полям сеелекционного экрана должен быть только к этого класса
* все остальные должны работать с его интрефейсом, тем самым уменьшается привязка кода к параметрам селекционного экрана
* появляется возможность подменить реальный селекционный экран, например, тестовым объектом, что пригодиться при юнит тестировании
*
CLASS lcl_selopt DEFINITION.

  PUBLIC SECTION.

    TYPES:
        mty_view_type TYPE char10.

    TYPES:
        mty_t_belnr_range TYPE RANGE OF bkpf-belnr.

    DATA:
         mt_belnr_range TYPE mty_t_belnr_range READ-ONLY.

    CONSTANTS:
      BEGIN OF mc_s_view_type,
        salv TYPE mty_view_type VALUE 'SALV',
        oalv TYPE mty_view_type VALUE 'OALV',
      END OF mc_s_view_type.

    METHODS:
      constructor,

      get_view_type
        RETURNING VALUE(rv_view_type) TYPE mty_view_type.

ENDCLASS.

CLASS lcl_selopt IMPLEMENTATION.
  METHOD get_view_type.
    CASE 'X'.
      WHEN p_salv.
        rv_view_type = mc_s_view_type-salv.
      WHEN p_oalv.
        rv_view_type = mc_s_view_type-oalv.
    ENDCASE.
  ENDMETHOD.

  METHOD constructor.
    mt_belnr_range[] = so_blr[].
  ENDMETHOD.
ENDCLASS.

*
* Событие
*
INTERFACE lif_event.
  TYPES:
       mty_id TYPE i.

  CONSTANTS:
    BEGIN OF mc_s_event_id, " идентификаторы всех возможныех в программе события
      start         TYPE mty_id VALUE 1,
      refresh_model TYPE mty_id VALUE 2,
    END OF mc_s_event_id.

  METHODS:
    get_id " получить идентификатор события
      RETURNING VALUE(rv_id) TYPE mty_id.
ENDINTERFACE.

*
* Модель паттерна MVC
*
INTERFACE lif_model.
  METHODS:
    set_selopt " передать в модель параметры для выборки
      IMPORTING
        io_selopt TYPE REF TO lcl_selopt,
    get_data " получить данные
      RETURNING VALUE(rr_data) TYPE REF TO data,
    refresh. " перечитать данные из источника

  EVENTS:
    on_changed. " событие о том что что-то изменилось в модели
ENDINTERFACE.

*
* Контроллер паттерна MVC
*
INTERFACE lif_controller.
  METHODS:
    init
      IMPORTING
        io_selopt TYPE REF TO lcl_selopt,
    handle_event
      IMPORTING
        io_event TYPE REF TO lif_event.
ENDINTERFACE.

*
* Представление паттерна MVC
*
INTERFACE lif_view.
  METHODS:
    display,
    set_model
      IMPORTING
        io_model TYPE REF TO lif_model,
    refresh.

  EVENTS:
    on_command
       EXPORTING VALUE(io_event) TYPE REF TO lif_event.
ENDINTERFACE.

*
* интерфейс для view умеющих работать с экраном
*
INTERFACE lif_screen.
  METHODS:
    pbo,
    pai
      IMPORTING
        iv_ok_code TYPE sy-ucomm.
ENDINTERFACE.

*
* интерфейс класса экрана
*
INTERFACE lif_screen_proxy.
  METHODS:
    register_view
      IMPORTING
        io_view TYPE REF TO lif_screen.
ENDINTERFACE.

*
* Комманда, абстракция позволяющая не складывать логику обработки функций в контроллер
*
INTERFACE lif_command.
  METHODS:
    execute.
ENDINTERFACE.

CLASS lcl_test_event DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
 .

  PUBLIC SECTION.
    METHODS:
      first_test FOR TESTING.
ENDCLASS.

*
* Простое событие без параметров
* если нужно передавать параметры, можн усложнить
*
CLASS lcl_event DEFINITION FRIENDS lcl_test_event.
  PUBLIC SECTION.
    INTERFACES:
      lif_event.

    METHODS:
      constructor
        IMPORTING
          iv_id TYPE lif_event=>mty_id.
  PROTECTED SECTION.
    DATA:
         mv_id TYPE lif_event=>mty_id.
ENDCLASS.

CLASS lcl_event IMPLEMENTATION.
  METHOD constructor.
    mv_id = iv_id.
  ENDMETHOD.
  METHOD lif_event~get_id.
    rv_id = mv_id.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_test_event IMPLEMENTATION.
  METHOD first_test.

    DATA:
      lv_x     TYPE lif_event=>mty_id,
      lo_event TYPE REF TO lcl_event.

    CREATE OBJECT lo_event
      EXPORTING
        iv_id = lif_event=>mc_s_event_id-start.

    lv_x = lo_event->lif_event~get_id( ).

    cl_aunit_assert=>assert_equals(
        exp                  = lif_event=>mc_s_event_id-start
        act                  = lv_x
        msg                  = 'Не возвращает переданный в конструктор идентификатор'
           ).
  ENDMETHOD.
ENDCLASS.

*
* Модель
*
CLASS lcl_model DEFINITION.
  PUBLIC SECTION.

    TYPES:
      mty_s_data TYPE zls_oop_mvc_alv_fcat,
      mty_t_data TYPE STANDARD TABLE OF mty_s_data WITH DEFAULT KEY.

    INTERFACES:
      lif_model.

  PROTECTED SECTION.
    DATA:
      mt_data      TYPE mty_t_data,
      mo_selopt    TYPE REF TO lcl_selopt,
      mv_need_load TYPE abap_bool.

    METHODS:
      select_data.

ENDCLASS.

CLASS lcl_model IMPLEMENTATION.

  METHOD lif_model~set_selopt.

    mo_selopt = io_selopt.

    mv_need_load = abap_true.

  ENDMETHOD.

  METHOD lif_model~refresh.
    mv_need_load = abap_true.

    select_data( ).

  ENDMETHOD.

*
* метод не должен принимать никаких пар аметров
* работает только на переменных объекта модели
*
  METHOD select_data.

    DATA:
          ls_data LIKE LINE OF mt_data.

    " к селекционному экрану обращаться через mo_selopt.

    IF mv_need_load <> abap_true.
      RETURN.
    ENDIF.

    mv_need_load = abap_false.

    CLEAR mt_data[].

    IF mo_selopt->mt_belnr_range IS INITIAL.
      SELECT
        belnr
        gjahr
        buzei
        dmbtr
*      budat
      INTO CORRESPONDING FIELDS OF TABLE mt_data
      FROM bseg
      UP TO 200 ROWS.
    ELSE.
      SELECT
        belnr
        gjahr
        buzei
        dmbtr
*      budat
      INTO CORRESPONDING FIELDS OF TABLE mt_data
      FROM bseg
      WHERE
        belnr IN mo_selopt->mt_belnr_range.
    ENDIF.

    RAISE EVENT lif_model~on_changed.

  ENDMETHOD.

  METHOD lif_model~get_data.

    select_data( ).

    GET REFERENCE OF mt_data INTO rr_data.

  ENDMETHOD.
ENDCLASS.

*
* Абстрактное представление, определяющее какими должны быть все представления в программе
* всё что наследуется от этого класса должно без проблем работать везде где указан тип lcl_view
*
CLASS lcl_view DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES:
      lif_view.

  PROTECTED SECTION.
    DATA:
         mo_model TYPE REF TO lif_model.
ENDCLASS.

CLASS lcl_view IMPLEMENTATION.
  METHOD lif_view~display.
    WRITE: 'Для данного вида отображения ещё нет реализации'.
  ENDMETHOD.

  METHOD lif_view~set_model.
    mo_model = io_model.
  ENDMETHOD.

  METHOD lif_view~refresh.
  ENDMETHOD.
ENDCLASS.

*
* Представление на основе SALV
*
CLASS lcl_view_salv DEFINITION INHERITING FROM lcl_view.
  PUBLIC SECTION.
    METHODS:
      lif_view~display REDEFINITION,
      lif_view~refresh REDEFINITION.

    METHODS:
      " Обработчик вызывается до нажатия стандартных функций
      on_before_salv_function FOR EVENT before_salv_function OF cl_salv_events IMPORTING e_salv_function,
      " Обработчик вызывается после нажатия стандартных функций
      on_after_salv_function  FOR EVENT after_salv_function  OF cl_salv_events IMPORTING e_salv_function,
      " Обработчик своих функций
      on_added_salv_function  FOR EVENT added_function       OF cl_salv_events IMPORTING e_salv_function.

  PROTECTED SECTION.
    DATA:
          mo_alv TYPE REF TO cl_salv_table.

    METHODS:
      config_columns
        IMPORTING
          io_columns TYPE REF TO cl_salv_columns,
      config_functions
        IMPORTING
          io_functions TYPE REF TO cl_salv_functions.
ENDCLASS.

CLASS lcl_view_salv IMPLEMENTATION.
  METHOD lif_view~display.

    DATA:
      lr_data   TYPE REF TO data,
      lo_events TYPE REF TO cl_salv_events_table.

    FIELD-SYMBOLS:
             <lt_data> TYPE STANDARD TABLE.

    lr_data = mo_model->get_data( ).
    IF lr_data IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN lr_data->* TO <lt_data>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    cl_salv_table=>factory( IMPORTING
                              r_salv_table = mo_alv
                            CHANGING
                              t_table   = <lt_data> ).

    " Подключаем стандартные функции
    mo_alv->set_screen_status( EXPORTING
                                report = sy-repid "'SAPLSALV_METADATA_STATUS'
                                pfstatus = 'SALV'
                                set_functions = cl_salv_model_base=>c_functions_all ).

    config_columns( mo_alv->get_columns( ) ).

    config_functions( mo_alv->get_functions( ) ).

    lo_events = mo_alv->get_event( ).
    SET HANDLER on_before_salv_function FOR lo_events.
    SET HANDLER on_added_salv_function FOR lo_events.
    SET HANDLER on_after_salv_function FOR lo_events.

    mo_alv->display( ).

  ENDMETHOD.

  METHOD lif_view~refresh.
    IF mo_alv IS NOT BOUND.
      RETURN.
    ENDIF.
    mo_alv->refresh( ).
  ENDMETHOD.

  METHOD config_columns.
    " здесь можно донастроить колонки ALV

    DATA:
         lo_column TYPE REF TO cl_salv_column.

    IF io_columns IS NOT BOUND.
      RETURN.
    ENDIF.

    io_columns->set_optimize( abap_true ).

*  TRY.
*    lo_column = go_columns->get_column( 'Имя поля' ).
*    " настраиваем текст
*    lo_column->set_long_text( 'Long long text' ).
*    lo_column->set_medium_text( 'Medium text' ).
*    lo_column->set_short_text( 'Short' ).
*
*  CATCH cx_salv_not_found.
*  ENDTRY.
*
  ENDMETHOD.

  METHOD config_functions.
    " здесь монжно настроить функции тулбара

    IF io_functions IS NOT BOUND.
      RETURN.
    ENDIF.

    io_functions->set_all( abap_true ).

  ENDMETHOD.

  " в этим методах может быть организована оработка стандартных комманд ALV
  METHOD on_before_salv_function.
  ENDMETHOD.

  METHOD on_after_salv_function.
  ENDMETHOD.

  METHOD on_added_salv_function.

    DATA:
          lo_event TYPE REF TO lcl_event.

    CASE e_salv_function.
      WHEN 'ZZREFRESH'.
        " в данном случае событие обновления инициализируется по кнопке на экране ALV
        " но может быть и каким-то другим способом сгенерировано, о котором контроллер не должен ни чего знать
        " поэтому нельзя контроллер привязывать напрямую к salv как обработчика её комманд
        CREATE OBJECT lo_event
          EXPORTING
            iv_id = lif_event=>mc_s_event_id-refresh_model.

        RAISE EVENT lif_view~on_command EXPORTING io_event = lo_event.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

*
* Класс представляющий связь view с экраном
* нельзя объединять с view так как, вью может менять в зависимости от параметров программы, а экран всегда остается один
*
CLASS lcl_screen_proxy_0100 DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.

    CONSTANTS:
       mc_alv_container_name TYPE char30 VALUE 'CONT1'.

    INTERFACES:
      lif_screen_proxy.

    CLASS-DATA:
          mv_ok_code  TYPE sy-ucomm.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_screen_proxy_0100.

    CLASS-METHODS:
      pbo,
      pai.

    METHODS:
      call_view_pbo,
      call_view_pai
        IMPORTING
          iv_ok_code TYPE sy-ucomm.

  PRIVATE SECTION.

    DATA:
        mo_view TYPE REF TO lif_screen.

    CLASS-DATA:
        mo_instance TYPE REF TO lcl_screen_proxy_0100.

ENDCLASS.

CLASS lcl_screen_proxy_0100 IMPLEMENTATION.

  METHOD lif_screen_proxy~register_view.
    " при необходимости использования крана несколькими view тут может усложниться логика регистрации
    mo_view = io_view.
  ENDMETHOD.

  METHOD get_instance.

    IF mo_instance IS NOT BOUND.
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.

  ENDMETHOD.

  METHOD pbo.
    DATA:
        lo_instance TYPE REF TO lcl_screen_proxy_0100.

    lo_instance = get_instance( ).

    lo_instance->call_view_pbo( ).

  ENDMETHOD.

  METHOD pai.
    DATA:
      lo_instance TYPE REF TO lcl_screen_proxy_0100,
      lv_ok_code  TYPE sy-ucomm.

    lo_instance = get_instance( ).

    lv_ok_code = mv_ok_code.
    CLEAR mv_ok_code.

    lo_instance->call_view_pai( lv_ok_code ).

  ENDMETHOD.

  METHOD call_view_pbo.
    IF mo_view IS BOUND.
      mo_view->pbo( ).
    ENDIF.
  ENDMETHOD.

  METHOD call_view_pai.
    IF mo_view IS BOUND .
      mo_view->pai( iv_ok_code ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*
* Представление на основе объектной ALV
*
CLASS lcl_view_oalv DEFINITION INHERITING FROM lcl_view.
  PUBLIC  SECTION.
    METHODS:
      lif_view~display REDEFINITION,
      lif_view~refresh REDEFINITION.

    INTERFACES:
      lif_screen.

    METHODS:
      constructor
        IMPORTING
          io_screen_proxy TYPE REF TO lif_screen_proxy,
      handle_command
        IMPORTING
          iv_ok_code TYPE sy-ucomm.

    METHODS:
      on_user_command         FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm.

  PROTECTED SECTION.

    DATA:
      mo_alv_grid      TYPE REF TO cl_gui_alv_grid,
      mo_container_alv TYPE REF TO cl_gui_custom_container.

    METHODS:
      create_fcat
        EXPORTING
          et_fcat TYPE lvc_t_fcat,
      create_toolbar_exclude
        EXPORTING
          et_exclude TYPE ui_functions,
      init_alv,
      refresh_alv.
ENDCLASS.

CLASS lcl_view_oalv IMPLEMENTATION.
  METHOD lif_view~display.
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD lif_view~refresh.
    refresh_alv( ).
  ENDMETHOD.

  METHOD lif_screen~pbo.

    SET PF-STATUS '0100'.

    IF mo_alv_grid IS NOT BOUND.
      init_alv( ).
    ELSE.
      refresh_alv( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_screen~pai.

    handle_command( iv_ok_code ).

  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).

    IF io_screen_proxy IS BOUND.
      io_screen_proxy->register_view( me ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_command.

    DATA:
          lo_event TYPE REF TO lcl_event.

    CASE iv_ok_code.
      WHEN 'BACK'.
        LEAVE TO SCREEN 0.
      WHEN 'ZZREFRESH'.

        CREATE OBJECT lo_event
          EXPORTING
            iv_id = lif_event=>mc_s_event_id-refresh_model.

        RAISE EVENT lif_view~on_command EXPORTING io_event = lo_event.
    ENDCASE.
  ENDMETHOD.

  METHOD init_alv.
    DATA:
      lt_fcat    TYPE lvc_t_fcat,
      ls_variant TYPE disvariant,
      ls_layout  TYPE lvc_s_layo,
      lt_exc     TYPE ui_functions,
      lr_data    TYPE REF TO data.

    FIELD-SYMBOLS:
               <lt_table> TYPE STANDARD TABLE.

* важно получить данные из модели до того как будет создана ALV
* иначе может образоваться цикл из событий
    lr_data = mo_model->get_data( ).
    IF lr_data IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN lr_data->* TO <lt_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF mo_container_alv IS NOT BOUND.
      CREATE OBJECT mo_container_alv
        EXPORTING
          container_name              = lcl_screen_proxy_0100=>mc_alv_container_name
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    CREATE OBJECT mo_alv_grid
      EXPORTING
        i_parent          = mo_container_alv   " Parent Container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    create_fcat(
      IMPORTING
        et_fcat = lt_fcat ).

    ls_variant-report = sy-repid.
    ls_variant-handle = 1.

    ls_layout-zebra = 'X'.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-stylefname = 'STYLE'.
    ls_layout-box_fname = 'MARK'.
    ls_layout-sel_mode = 'A'.
*    ls_layout-no_rowmark = 'X'.

    create_toolbar_exclude(
       IMPORTING
         et_exclude = lt_exc
    ).

    mo_alv_grid->register_edit_event(
        EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    mo_alv_grid->register_edit_event(
         EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    CALL METHOD mo_alv_grid->set_table_for_first_display
      EXPORTING
        i_buffer_active               = ''
        is_variant                    = ls_variant    " Layout
        i_save                        = 'A'    " Save Layout
        is_layout                     = ls_layout    " Layout
        it_toolbar_excluding          = lt_exc
      CHANGING
        it_outtab                     = <lt_table>
        it_fieldcatalog               = lt_fcat    " Field Catalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE  ID sy-msgid
               TYPE 'S'
               NUMBER sy-msgno
               DISPLAY LIKE sy-msgty
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SET HANDLER on_user_command       FOR mo_alv_grid.

  ENDMETHOD.

  METHOD refresh_alv.
    DATA:
      ls_stable TYPE lvc_s_stbl.

    ls_stable-col = 'X'.
    ls_stable-row = 'X'.

    IF mo_alv_grid IS NOT BOUND.
      RETURN.
    ENDIF.

    mo_alv_grid->refresh_table_display(
      is_stable = ls_stable ).
  ENDMETHOD.

  METHOD on_user_command.
    handle_command( e_ucomm ).
  ENDMETHOD.

  METHOD create_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZLS_OOP_MVC_ALV_FCAT'
      CHANGING
        ct_fieldcat            = et_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      " TODO тут надо обработать ошибку корректно
    ENDIF.

  ENDMETHOD.

  METHOD create_toolbar_exclude.
    APPEND:
        cl_gui_alv_grid=>mc_fc_find               TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_loc_copy           TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_loc_copy_row       TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_loc_cut            TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_detail             TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_loc_undo           TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_loc_paste          TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_loc_paste_new_row  TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_refresh            TO  et_exclude
*      , cl_gui_alv_grid=>mc_fc_sum                TO  et_exclude
*      , cl_gui_alv_grid=>mc_fc_subtot             TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_views              TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_view_crystal       TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_view_excel         TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_view_grid          TO  et_exclude
      , cl_gui_alv_grid=>mc_mb_export             TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_graph              TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_info               TO  et_exclude
*      , cl_gui_alv_grid=>mc_mb_sum                TO  et_exclude
*      , cl_gui_alv_grid=>mc_mb_variant            TO et_exclude
      , cl_gui_alv_grid=>mc_fc_loc_move_row       TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_loc_append_row     TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_check              TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_loc_insert_row     TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_loc_delete_row     TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_sort_asc           TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_sort_dsc           TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_call_report        TO  et_exclude
*      , cl_gui_alv_grid=>mc_mb_sum                TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_print              TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_print_back         TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_print_prev         TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_refresh            TO  et_exclude
*      , cl_gui_alv_grid=>mc_fc_filter             TO  et_exclude
      , cl_gui_alv_grid=>mc_fc_find_more          TO  et_exclude
      .
  ENDMETHOD.

ENDCLASS.

*
* Комманда обновления модели
* может показаться что для такой простой комманды заводить класс слишком избыточно
* и это так, но комманды бывают и не такими простыми, это только пример
*
CLASS lcl_command_refresh DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_command.

    METHODS:
      constructor
        IMPORTING
          io_model TYPE REF TO lif_model.

  PROTECTED SECTION.
    DATA:
        mo_model TYPE REF TO lif_model.
ENDCLASS.

CLASS lcl_command_refresh IMPLEMENTATION.

  METHOD constructor.
    mo_model = io_model.
  ENDMETHOD.

  METHOD lif_command~execute.
    " тут можно делать с моделью что угодно, логика уже не будет нагружать контроллер и создавать
    " дополнительные зависимости между контроллером и используемыми в комманде объектами
    IF mo_model IS BOUND.
      mo_model->refresh( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*
* Контроллер
* Не должен знать о внутренностях view и model
* может оперировать только абстракциями view и model
* деталзиция действий должна быть в классах-обработчиках команд
* методы create_view и create_model в идеале должны быть вынесены в фабрику
*
CLASS lcl_controller DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_controller.

    METHODS:
      handle_command FOR EVENT on_command OF lif_view
        IMPORTING
            io_event sender,
      handle_model_changed FOR EVENT on_changed OF lif_model.

  PROTECTED SECTION.
    DATA:
      mo_view   TYPE REF TO lif_view,
      mo_model  TYPE REF TO lif_model,
      mo_selopt TYPE REF TO lcl_selopt.

    METHODS:
      create_view
        IMPORTING
                  io_selopt      TYPE REF TO lcl_selopt
        RETURNING VALUE(ro_view) TYPE REF TO lif_view,

      create_model
        IMPORTING
                  io_selopt       TYPE REF TO lcl_selopt
        RETURNING VALUE(ro_model) TYPE REF TO lif_model.
ENDCLASS.

CLASS lcl_controller IMPLEMENTATION.
  METHOD lif_controller~init.

    mo_selopt = io_selopt.

    mo_view = create_view( mo_selopt ).

    mo_model = create_model( mo_selopt ).

    mo_view->set_model( mo_model ).

    SET HANDLER handle_command FOR mo_view.
    SET HANDLER handle_model_changed FOR mo_model.

  ENDMETHOD.

  METHOD lif_controller~handle_event.

    DATA:
          lo_command TYPE REF TO lif_command.

    IF io_event IS NOT BOUND.
      RETURN.
    ENDIF.

    CASE io_event->get_id( ).
      WHEN lif_event=>mc_s_event_id-start.
        IF mo_view IS BOUND.
          mo_view->display( ).
        ELSE.
          WRITE: 'Способ отображения не выбран.'. " TODO здесь должно быть сообщение об ошибке
        ENDIF.
      WHEN lif_event=>mc_s_event_id-refresh_model.

        CREATE OBJECT lo_command TYPE lcl_command_refresh
          EXPORTING
            io_model = mo_model.
    ENDCASE.

    IF lo_command IS BOUND.
      lo_command->execute( ).
    ENDIF.
  ENDMETHOD.

  METHOD create_view.

    DATA:
         lo_screen_proxy TYPE REF TO lif_screen_proxy.

    " В зависимости от того что выбрано на селекционном экране
    " выбирается вид отображения
    CASE io_selopt->get_view_type( ).
      WHEN lcl_selopt=>mc_s_view_type-salv.
        CREATE OBJECT ro_view TYPE lcl_view_salv.
      WHEN lcl_selopt=>mc_s_view_type-oalv.
        lo_screen_proxy = lcl_screen_proxy_0100=>get_instance( ).
        CREATE OBJECT ro_view TYPE lcl_view_oalv
          EXPORTING
            io_screen_proxy = lo_screen_proxy.
    ENDCASE.
  ENDMETHOD.

  METHOD create_model.
    " Тут можно поменять источник данных на другой
    CREATE OBJECT ro_model TYPE lcl_model.

    ro_model->set_selopt( io_selopt ).

  ENDMETHOD.

  METHOD handle_command.

    lif_controller~handle_event( io_event ).

  ENDMETHOD.

  METHOD handle_model_changed.
    IF mo_view IS BOUND.
      mo_view->refresh( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      mo_selopt          TYPE REF TO lcl_selopt READ-ONLY,
      mo_main_controller TYPE REF TO lif_controller.

    CLASS-METHODS:
      start_of_selection,
      end_of_selection.

  PROTECTED SECTION.
    CLASS-METHODS:
      create_selopt.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.
  METHOD start_of_selection.

    create_selopt( ).

    CREATE OBJECT mo_main_controller TYPE lcl_controller.

    mo_main_controller->init( mo_selopt ).

  ENDMETHOD.

  METHOD end_of_selection.

    DATA:
         lo_start TYPE REF TO lcl_event.

    CREATE OBJECT lo_start
      EXPORTING
        iv_id = lif_event=>mc_s_event_id-start.

    mo_main_controller->handle_event( lo_start ).

  ENDMETHOD.

  METHOD create_selopt.

    CREATE OBJECT mo_selopt.

  ENDMETHOD.
ENDCLASS.
