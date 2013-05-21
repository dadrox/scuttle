package com.dadrox.scuttle.twitter

import org.fictus.Fictus
import org.junit.Test
import com.dadrox.scuttle.time.{ Duration, Time }
import com.twitter.util.{ Duration => TwitterDuration, Time => TwitterTime }

class ConvertersTest extends Fictus {
    @Test
    def scuttleTimeToTwitter {
        import com.dadrox.scuttle.twitter.conversions.scuttleTimeToTwitterTime

        val time: TwitterTime = Time.Epoch
        val twitter = TwitterTime.epoch

        time mustEqual twitter
        twitter mustEqual time
    }

}