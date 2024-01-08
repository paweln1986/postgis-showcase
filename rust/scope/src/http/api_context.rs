use crate::scopes::scope_repository::DynScopeRepository;
use crate::antennas::antenna_repository::DynAntennaRepository;

#[derive(Clone)]
pub struct ApiContext {
    pub scope_store: DynScopeRepository,
    pub antenna_store: DynAntennaRepository
}