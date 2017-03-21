## Explorer Backend API

Currently, the explorer's API provides a series of methods to work with `cardano-sl`. The `servant` Haskell library that provides a modular approach to API-building was used. This library uses combinators to both build atomic HTTP actions and to glue these atomic methods together to form larger and more complete APIs.

If the event requests fail, there is a `ExplorerError` type, which is simply a wrapper over `Text` to show what happened.

Currently, the explorer's API supports the following operations (see Comments below):

## GET /api/addresses/summary/:address

#### Description

Get address summary

#### Authentication



Clients must supply the following data


#### Captures:

- *address*: Address

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{"Left":"This is an example error"}
```

- Sample address summary

```javascript
{"Right":{"caAddress":"1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv","caTxNum":0,"caBalance":{"getCoin":0},"caTxList":[]}}
```

## GET /api/blocks/last

#### Description

Get last block

#### Authentication



Clients must supply the following data


#### GET Parameters:

- limit
     - **Values**: *0, 100*
     - **Description**: Max numbers of transactions to return

- offset
     - **Values**: *0, 100*
     - **Description**: Offset this many transactions


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{"Left":"This is an example error"}
```

- 

```javascript
{"Right":[]}
```

- Sample block entry

```javascript
{"Right":[{"cbeBlkHash":"75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d","cbeHeight":10,"cbeTimeIssued":null,"cbeTxNum":0,"cbeTotalSent":{"getCoin":0},"cbeSize":390,"cbeRelayedBy":null}]}
```

- Sample block entry, Sample block entry

```javascript
{"Right":[{"cbeBlkHash":"75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d","cbeHeight":10,"cbeTimeIssued":null,"cbeTxNum":0,"cbeTotalSent":{"getCoin":0},"cbeSize":390,"cbeRelayedBy":null},{"cbeBlkHash":"75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d","cbeHeight":10,"cbeTimeIssued":null,"cbeTxNum":0,"cbeTotalSent":{"getCoin":0},"cbeSize":390,"cbeRelayedBy":null}]}
```

- Sample block entry, Sample block entry, Sample block entry

```javascript
{"Right":[{"cbeBlkHash":"75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d","cbeHeight":10,"cbeTimeIssued":null,"cbeTxNum":0,"cbeTotalSent":{"getCoin":0},"cbeSize":390,"cbeRelayedBy":null},{"cbeBlkHash":"75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d","cbeHeight":10,"cbeTimeIssued":null,"cbeTxNum":0,"cbeTotalSent":{"getCoin":0},"cbeSize":390,"cbeRelayedBy":null},{"cbeBlkHash":"75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d","cbeHeight":10,"cbeTimeIssued":null,"cbeTxNum":0,"cbeTotalSent":{"getCoin":0},"cbeSize":390,"cbeRelayedBy":null}]}
```

## GET /api/blocks/summary/:hash

#### Description

Get block summary

#### Authentication



Clients must supply the following data


#### Captures:

- *hash*: Hash

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{"Left":"This is an example error"}
```

- Sample block summary

```javascript
{"Right":{"cbsEntry":{"cbeBlkHash":"75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d","cbeHeight":10,"cbeTimeIssued":null,"cbeTxNum":0,"cbeTotalSent":{"getCoin":0},"cbeSize":390,"cbeRelayedBy":null},"cbsPrevHash":"d36710c918da4c4a3e0ff42e1049d81cc7bcbacc789c8583ea1c9afd8da3c24e","cbsNextHash":"d3bb988e57356b706f7b8f1fe29591ab0d1bdfac4aa08836475783973e4cf7c1","cbsMerkleRoot":"69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"}}
```

## GET /api/blocks/txs/:hash

#### Description

Get block transactions

#### Authentication



Clients must supply the following data


#### Captures:

- *hash*: Hash

#### GET Parameters:

- limit
     - **Values**: *0, 100*
     - **Description**: Max numbers of transactions to return

- offset
     - **Values**: *0, 100*
     - **Description**: Offset this many transactions


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{"Left":"This is an example error"}
```

- 

```javascript
{"Right":[]}
```

- Sample transaction entry

```javascript
{"Right":[{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}}]}
```

- Sample transaction entry, Sample transaction entry

```javascript
{"Right":[{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}},{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}}]}
```

- Sample transaction entry, Sample transaction entry, Sample transaction entry

```javascript
{"Right":[{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}},{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}},{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}}]}
```

## GET /api/search/:hash

#### Description

Search for transaction, block or address.

#### Authentication



Clients must supply the following data


#### Captures:

- *hash*: Search id by which the user can find address, block or transaction

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{"Left":"This is an example error"}
```

- Sample search result, address found

```javascript
{"Right":{"tag":"AddressFound","contents":{"caAddress":"1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv","caTxNum":0,"caBalance":{"getCoin":0},"caTxList":[]}}}
```

## GET /api/txs/last

#### Description

Get last transaction

#### Authentication



Clients must supply the following data


#### GET Parameters:

- limit
     - **Values**: *0, 100*
     - **Description**: Max numbers of transactions to return

- offset
     - **Values**: *0, 100*
     - **Description**: Offset this many transactions


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{"Left":"This is an example error"}
```

- 

```javascript
{"Right":[]}
```

- Sample transaction entry

```javascript
{"Right":[{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}}]}
```

- Sample transaction entry, Sample transaction entry

```javascript
{"Right":[{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}},{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}}]}
```

- Sample transaction entry, Sample transaction entry, Sample transaction entry

```javascript
{"Right":[{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}},{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}},{"cteId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","cteTimeIssued":1512259200,"cteAmount":{"getCoin":33333}}]}
```

## GET /api/txs/summary/:txid

#### Description

Get transaction summary

#### Authentication



Clients must supply the following data


#### Captures:

- *txid*: Transaction id

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{"Left":"This is an example error"}
```

- Sample transaction summary

```javascript
{"Right":{"ctsId":"b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02","ctsTxTimeIssued":1512259200,"ctsBlockTimeIssued":null,"ctsBlockHeight":11,"ctsRelayedBy":null,"ctsTotalInput":{"getCoin":33333},"ctsTotalOutput":{"getCoin":33333},"ctsFees":{"getCoin":0},"ctsInputs":[["1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv",{"getCoin":33333}]],"ctsOutputs":[["1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",{"getCoin":33333}]]}}
```

