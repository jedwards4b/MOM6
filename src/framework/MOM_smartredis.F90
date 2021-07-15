!> Contains routines necessary to initialize the SmartRedis client
module MOM_smartredis

use MOM_cpu_clock,        only : cpu_clock_id, cpu_clock_begin, cpu_clock_end, CLOCK_ROUTINE
use MOM_error_handler,    only : MOM_error, FATAL, WARNING, NOTE, MOM_mesg, is_root_pe
use MOM_file_parser,      only : read_param, get_param, log_version, param_file_type
use smartredis_client,    only : client_type

implicit none; private

public :: client_type

contains

subroutine smartredis_init(param_file, client)
  type(param_file_type), intent(in   ) :: param_file !< Parameter file structure
  type(client_type),     intent(inout) :: client     !< Client used to communicate with the SmartRedis database

  character(len=40) :: mdl = "MOM_SMARTREDIS"
  logical :: use_smartredis
  logical :: use_smartredis_cluster
  integer :: id_client_init

  call get_param(param_file, mdl, "USE_SMARTREDIS",  use_smartredis, &
  		 "If true, initialize the client used to communcicate "//&
		 "with the SmartRedis database", default=.false.)

  if (use_smartredis) then
    call get_param(param_file, mdl, "USE_SMARTREDIS_CLUSTER",  use_smartredis_cluster, &
    		   "If true, the SmartRedis database is distributed over multiple nodes.",&
		   default=.true.)
    id_client_init = cpu_clock_id('(SMARTREDIS client init)', grain=CLOCK_ROUTINE)
    call cpu_clock_begin(id_client_init)
    call client%initialize(use_smartredis_cluster)
    call cpu_clock_end(id_client_init)
  endif

end subroutine smartredis_init

end module MOM_smartredis

