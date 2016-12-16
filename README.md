# Roman Emperors from 26 BC to 395 AD

## Data Notes

Below are some notes on how `birth`, `death`, `reign.start`, and `reign.end`. were calculated:

* If the *year* of the date was known, but not the *day* or *month*, the start of the year was taken.
* If the *year and month* of the date was known, but not the *day*, the middle of the month was taken (14 for February, 15 for other months).
* If *two dates were given*, due to inconsistencies with ancient sources, then:
    * In the case it was `birth` or `reign.start`, the earlier date was chosen.
    * In the case it was `death` or `reign.end`, the later date was chosen.
* If the *year and one of two possible months* of the date was known, but not the day, the first date of the second month was chosen (middle of the two options).
* If the date was BCE, the following actions were performed:
    * The year was made positive
    * The year had one year subtracted from its absolute value, to make it consistent with [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Years) (e.g. 1AD = 0001, 1BC = 0000, 2BC = 0001, 3BC = 0002, ...).
    * The "notes" column will state BCE date.
    * The code in `emperors.R` was modified to invert the sign of that specific value (e.g. 1AD = 0001, 1BC = 0000, 2BC = -001, 3BC = -002, ...).
