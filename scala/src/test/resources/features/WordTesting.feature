Feature: Wold testing

    As a developer/qa
    I want to be capable to handle world's state properly
    So that I write scenarios with proper reference to states

    Scenario: General test of state State handling in world
        Given User U0 exists in the system with name "Alice" - GENERAL

    Scenario: Test of State handling between different sentences
        Given User U0 exists in the system with name "Alice"
        And User U1 exists in the system with name "Bob"
        When Users U0,U1 names are concatenated in given order
        Then the result is "AliceBob" as String

##    Failure is expected - run manually
#    @manual
#    Scenario: missing state handling
#        Given User U0 exists in the system with name "Alice" and age A0
#        And User U1 exists in the system with name "Bob" and age A1
#        When Users U0,U2 names are concatenated in given order
#
##    Failure is expected - run manually
#    @manual
#    Scenario: duplicated state handling
#        Given User U0 exists in the system with name "Alice" and age A0
#        And User U0 exists in the system with name "Bob" and age A1
#        When Users U0,U1 names are concatenated in given order

    @currdev
    Scenario: derivative object testing scenario
        Given the cumulative B0 limit rule L exists in DC limit management with amount LA with currency C1
        And User U1 exists in the system with name "Bob"
        And User U1 has Account A0 is given with id number "11111111-22222222-33333333" with balance 100 in currency USD
        When User U1 initiate transaction T0 from Account A0 with amount AM0 in currency USD
        Then Transaction T0 is accepted if amount LA greater than its amount
        And Transaction T0 is accepted if its amount less than 100

