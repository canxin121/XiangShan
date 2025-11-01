# XiangShan Investigation & Enhancements Summary

## RV32 Capability Inquiry
- Confirmed that current XiangShan codebase targets RV64 only.  
  - `top/Configs.scala` fixes `XLen` to 64 with no alternative configs.  
  - Documentation and build scripts lack RV32 references.  
  - RV32 support would require substantial new configuration work.

## Logging/Trace Capabilities Review
- Examined existing tracing (ROB trace, DiffTest modules, XSLog infrastructure).  
- Identified that per-instruction register writes and exceptions already traceable via `--dump-commit-trace`.  
- Found gap: store events only used for diff checking, not user-visible logging.

## New Store Trace Feature
- Added `StoreTrace` data path in DiffTest to capture PC/addr/data/mask/robidx.  
- Introduced `--dump-store-trace` emulator flag; logging respects `--log-begin/--log-end`.  
- Updated README with usage guidance.  
- Behaviour mirrors existing commit trace flow (bounded buffer, optional streaming output).

## Notes
- No automated tests run; feature verified through code inspection.  
- Potential future improvement: conditional buffering when logging disabled to reduce runtime overhead.
