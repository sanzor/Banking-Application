ex_banking
==========

A simple banking application supporting the following operations over user acocunts:

- create user account
- get balance from target account in currency
- deposit amount to target account in currency
- withdraw amount from target account in currency
- send amount from User1 to User 2 in currency

Application features:

- multiple accounts can be serviced concurrently
- a number of MAX operations can be done at any given time over a target account


Build
-----

$ rebar3 compile
