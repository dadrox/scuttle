package com.dadrox.scuttle.tz

import Io._

class OlsonTimeZoneDataProcessor(
        parser: OlsonTimeZoneDataParser) {

    def process(path: String) = {
        val data = parser.fromDirectory(path)
    }
}