Feature: Insert filter result
  In order to compose some kind of text report
  As a user
  I want to download list of issues from jira into current buffer

  Background:
    Given base-url points to "https://bugreports.qt.io"
    And "key = QTWEBSITE-741" added to quick-filters as "test single issue"
    And authentication is disabled

  # Action chain magic from
  # https://github.com/antham/helm-backup/blob/master/features/helm-backup.feature
  Scenario: Non empty result
    Given I start an action chain
      And I press "M-x"
      And I type "jira-show-filter-result"
      Then I press "RET"
      And I type "test single issue"
      And I press "RET"
    And I execute the action chain
    Then Eventually I should see "QTWEBSITE-741" in buffer
