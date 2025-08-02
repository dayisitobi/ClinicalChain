# ClinicalChain

A decentralized clinical trial management and patient enrollment protocol for transparent medical research on Stacks blockchain.

## Features

- Clinical trial creation and patient enrollment management
- Patient participation with compensation and safety bond mechanisms
- Adherence tracking and completion validation
- Clinical outcome reporting with hash verification
- Comprehensive trial management statistics

## Smart Contract Functions

### Public Functions
- `create-clinical-trial` - Create new clinical trial for patient enrollment
- `enroll-trial` - Enroll in trial with safety bond and compensation
- `update-adherence` - Update patient adherence score
- `report-outcomes` - Report clinical outcomes with validation
- `deactivate-trial` - Deactivate trial (principal investigator only)

### Read-Only Functions
- `get-clinical-trial` - Get trial details and metadata
- `get-patient-enrollment` - Get enrollment record
- `get-patient-access` - Get patient's trial access
- `get-clinical-outcome` - Get outcome record
- `is-patient-reported` - Check if patient reported outcomes
- `get-trial-stats` - Get comprehensive trial statistics
- `get-platform-stats` - Get platform-wide statistics
- `calculate-trial-cost` - Calculate total trial participation cost

## Clinical Features
- Multi-phase trial support
- Medical condition classification
- Adherence-based completion tracking
- Outcome verification system

## Usage

Deploy the contract to create a clinical trial management platform where investigators can create trials, patients can enroll with safety mechanisms, and outcomes can be verified.

## License

MIT