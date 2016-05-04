# Dimensions

The following dimensions have been identified:

### Funding Channel
A funding channel is a set of 1 or more investors that are grouped together for the purposes of allocating their investment capital to products based on a set of criteria and concentration limits.  Each institutional investor is (thus far) a singleton member in its funding channel, but we group all retail investors as a single funding channel.  Each channel is uniquely identified by a UUID, and has an associated name and allocation percentage.  A channel can be "referral only", meaning only loans referred by the investor(s) in the channel can be allocated to that channel.  A channel can have a scaleable allocation percentage, meaning that the raw percentage is scaled up at the time of allocation decisioning if the pool of eligible channels have a total allocation percentage less than 100%.

Sample Data

| id | Funding Channel UUID | Name                | Allocation Percentage | Referral Only? | Scale Allocation Percentage? |
|----|----------------------|---------------------|-----------------------|----------------|------------------------------|
| 1  | abc123               | Make FC Great Again | 0.3                   | True           | False                        |
| 2  | xyz987               | Acme Funding        | .4                    | False          | False                        |
| 3  | foobar               | Retail Investors    | .2                    | False          | True                         |

### Listing
TBD

### Eligibility Criterion
TBD

### Concentration Limit
TBD
