-module(ems_bus_sup).

-behaviour(supervisor).

-include("../include/ems_schema.hrl").

%% Supervisor callbacks
-export([start_link/1, init/1]).
-export([start_process/1, start_process_sup/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	KernelServices = ems_catalog_loader:list_kernel_catalog(),
    PoolSpecs = lists:map(
		fun(S = #service{name = WorkerName, 
						  pool_size = PoolSize, 
						  pool_max = PoolMax, 
						  module = Worker}) ->
				WorkerNameAtom = list_to_atom(binary_to_list(WorkerName)),
				case PoolMax == 1 of
					true -> 
						{WorkerNameAtom,
							{?MODULE, start_process, [[Worker, start, S]]},
							permanent, 10000, worker,  [WorkerNameAtom]
						};
					false ->
						{WorkerNameAtom, 
							{?MODULE, start_process_sup, [[poolboy, start_link, [[{strategy, fifo},
																				  {name, {local, WorkerNameAtom}},
																				  {worker_module, Worker},
																				  {size, PoolSize},
																				  {max_overflow, PoolMax - PoolSize}], S]]]},
							 permanent, 5000, worker, [poolboy]
						}
				end
		end, KernelServices),
	{ok, {{one_for_one, 10, 10}, PoolSpecs}}.
	

start_process([Module, Function, Args]) ->
	Reply = apply(Module, Function, [Args]),
	ems_logger:info("Start ~p.", [Module]),
	Reply.
	
start_process_sup([Module, Function, Args = [[_,
											  {name, {local, WorkerNameAtom}},
											  _, 
											  {size, PoolSize}, 
											  {max_overflow, PoolMax}], 
											_]]) ->
	Reply = apply(Module, Function, Args),
	ems_logger:info("Start ~p with ~p workers (Max ~p)", [WorkerNameAtom, PoolSize, PoolMax]),
	Reply.

