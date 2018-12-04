(ns advent-of-code.input
  (:require [clojure.string :as str]))

(def day4 (str/split-lines
           "[1518-11-07 00:21] falls asleep
[1518-05-18 00:57] wakes up
[1518-09-24 00:19] falls asleep
[1518-04-26 00:21] falls asleep
[1518-08-09 00:28] wakes up
[1518-06-05 00:02] wakes up
[1518-03-06 00:53] falls asleep
[1518-08-12 00:04] falls asleep
[1518-05-13 00:47] falls asleep
[1518-07-27 00:04] falls asleep
[1518-03-07 00:04] Guard #1823 begins shift
[1518-03-15 23:57] Guard #2909 begins shift
[1518-04-07 00:17] falls asleep
[1518-07-01 00:44] wakes up
[1518-09-09 00:43] falls asleep
[1518-07-04 00:53] wakes up
[1518-07-12 00:21] falls asleep
[1518-09-09 00:57] wakes up
[1518-04-22 00:30] falls asleep
[1518-08-30 00:00] Guard #1279 begins shift
[1518-09-25 23:52] Guard #2657 begins shift
[1518-05-03 00:40] wakes up
[1518-06-28 00:00] Guard #2777 begins shift
[1518-06-29 00:26] falls asleep
[1518-09-24 00:57] wakes up
[1518-05-19 00:52] wakes up
[1518-02-27 00:59] wakes up
[1518-03-31 00:22] falls asleep
[1518-06-27 00:52] wakes up
[1518-08-03 00:41] falls asleep
[1518-04-09 00:51] wakes up
[1518-08-26 00:06] falls asleep
[1518-03-28 00:55] wakes up
[1518-10-10 00:56] falls asleep
[1518-07-25 23:56] Guard #151 begins shift
[1518-08-28 00:37] wakes up
[1518-11-02 00:42] wakes up
[1518-11-13 00:40] falls asleep
[1518-07-25 00:14] falls asleep
[1518-08-03 23:52] Guard #157 begins shift
[1518-08-14 00:02] Guard #1877 begins shift
[1518-05-01 00:19] falls asleep
[1518-09-07 00:12] falls asleep
[1518-05-10 00:30] falls asleep
[1518-05-21 00:02] Guard #1823 begins shift
[1518-11-14 00:48] wakes up
[1518-05-02 00:54] falls asleep
[1518-07-06 00:52] wakes up
[1518-06-16 00:18] falls asleep
[1518-10-16 23:57] Guard #1171 begins shift
[1518-11-17 23:49] Guard #1171 begins shift
[1518-04-15 00:57] wakes up
[1518-09-27 00:59] wakes up
[1518-06-20 00:37] falls asleep
[1518-04-27 23:59] Guard #83 begins shift
[1518-09-10 23:59] Guard #3011 begins shift
[1518-10-02 00:51] wakes up
[1518-07-04 00:01] Guard #1823 begins shift
[1518-10-03 00:58] wakes up
[1518-04-27 00:59] wakes up
[1518-04-21 00:55] wakes up
[1518-03-26 00:58] wakes up
[1518-07-06 00:43] falls asleep
[1518-04-29 00:03] Guard #2777 begins shift
[1518-05-23 23:58] Guard #263 begins shift
[1518-11-09 00:50] wakes up
[1518-04-07 00:49] falls asleep
[1518-03-06 00:35] falls asleep
[1518-05-18 00:56] falls asleep
[1518-06-28 00:26] falls asleep
[1518-07-14 23:58] Guard #1823 begins shift
[1518-10-05 00:48] wakes up
[1518-04-26 00:25] wakes up
[1518-11-12 00:54] wakes up
[1518-03-14 00:04] Guard #2729 begins shift
[1518-09-22 00:11] falls asleep
[1518-08-31 00:51] wakes up
[1518-06-10 00:36] wakes up
[1518-09-25 00:41] wakes up
[1518-06-23 00:18] falls asleep
[1518-10-23 00:43] falls asleep
[1518-11-03 23:57] Guard #151 begins shift
[1518-03-15 00:09] falls asleep
[1518-06-26 00:57] wakes up
[1518-07-20 00:58] wakes up
[1518-10-29 00:43] falls asleep
[1518-11-22 00:10] falls asleep
[1518-10-27 00:28] falls asleep
[1518-10-30 00:44] wakes up
[1518-05-19 00:02] Guard #2909 begins shift
[1518-08-25 00:00] Guard #2657 begins shift
[1518-09-22 00:59] wakes up
[1518-11-11 00:05] falls asleep
[1518-10-17 00:53] wakes up
[1518-03-18 23:57] Guard #1823 begins shift
[1518-04-06 00:52] wakes up
[1518-04-19 00:02] falls asleep
[1518-07-18 00:02] Guard #1481 begins shift
[1518-03-30 00:00] Guard #1481 begins shift
[1518-09-29 00:40] wakes up
[1518-08-14 00:26] wakes up
[1518-09-13 00:03] Guard #1823 begins shift
[1518-10-26 00:03] Guard #2729 begins shift
[1518-05-19 00:12] falls asleep
[1518-05-17 00:50] falls asleep
[1518-09-25 00:28] falls asleep
[1518-05-02 00:57] wakes up
[1518-05-18 00:30] falls asleep
[1518-03-31 00:30] wakes up
[1518-06-17 00:54] wakes up
[1518-08-31 23:56] Guard #3011 begins shift
[1518-06-30 00:01] falls asleep
[1518-10-08 00:23] wakes up
[1518-05-07 00:03] wakes up
[1518-10-03 00:53] falls asleep
[1518-10-18 00:37] falls asleep
[1518-10-03 00:19] falls asleep
[1518-06-03 00:17] falls asleep
[1518-09-23 00:57] wakes up
[1518-08-29 00:47] wakes up
[1518-10-20 00:43] wakes up
[1518-03-18 00:39] falls asleep
[1518-07-17 00:45] falls asleep
[1518-11-21 00:50] wakes up
[1518-11-19 00:52] falls asleep
[1518-10-04 00:41] falls asleep
[1518-06-13 00:23] falls asleep
[1518-09-18 00:00] falls asleep
[1518-07-13 00:19] falls asleep
[1518-04-13 00:10] falls asleep
[1518-06-20 00:58] wakes up
[1518-08-19 00:53] wakes up
[1518-07-10 00:22] wakes up
[1518-09-21 00:09] falls asleep
[1518-09-23 00:34] falls asleep
[1518-09-14 00:37] falls asleep
[1518-06-10 00:40] falls asleep
[1518-03-12 00:59] wakes up
[1518-03-27 23:59] Guard #1481 begins shift
[1518-09-23 00:06] falls asleep
[1518-10-13 00:00] Guard #197 begins shift
[1518-06-02 23:56] Guard #571 begins shift
[1518-10-07 00:11] falls asleep
[1518-07-23 00:41] wakes up
[1518-11-19 00:03] Guard #157 begins shift
[1518-08-06 00:04] Guard #2777 begins shift
[1518-09-02 00:56] wakes up
[1518-09-13 23:59] Guard #3089 begins shift
[1518-06-21 00:45] falls asleep
[1518-07-10 00:47] wakes up
[1518-07-01 00:25] falls asleep
[1518-07-16 00:01] falls asleep
[1518-08-17 00:19] falls asleep
[1518-08-29 00:56] wakes up
[1518-10-01 00:45] wakes up
[1518-09-29 00:59] wakes up
[1518-09-16 00:56] wakes up
[1518-03-10 00:58] wakes up
[1518-05-11 00:02] Guard #263 begins shift
[1518-03-17 23:59] Guard #1697 begins shift
[1518-05-07 00:51] wakes up
[1518-09-06 00:05] falls asleep
[1518-04-15 00:33] wakes up
[1518-11-02 00:58] wakes up
[1518-07-31 00:56] falls asleep
[1518-07-26 23:52] Guard #1877 begins shift
[1518-09-13 00:44] wakes up
[1518-06-20 00:38] wakes up
[1518-03-07 00:06] falls asleep
[1518-07-21 23:50] Guard #2657 begins shift
[1518-05-21 00:55] wakes up
[1518-03-12 23:58] Guard #3089 begins shift
[1518-03-07 00:58] wakes up
[1518-06-02 00:59] wakes up
[1518-08-05 00:22] falls asleep
[1518-07-17 00:55] wakes up
[1518-11-20 23:57] Guard #2729 begins shift
[1518-04-07 00:04] Guard #151 begins shift
[1518-10-29 00:34] falls asleep
[1518-10-07 00:27] wakes up
[1518-10-18 00:47] wakes up
[1518-05-07 00:02] falls asleep
[1518-07-20 00:27] falls asleep
[1518-09-14 00:48] wakes up
[1518-09-02 00:03] Guard #1279 begins shift
[1518-09-22 00:41] wakes up
[1518-06-14 00:02] falls asleep
[1518-09-01 00:52] wakes up
[1518-05-26 00:31] wakes up
[1518-09-06 00:59] wakes up
[1518-09-17 23:52] Guard #157 begins shift
[1518-02-28 00:26] wakes up
[1518-07-17 00:03] Guard #197 begins shift
[1518-04-22 00:44] wakes up
[1518-04-25 00:18] falls asleep
[1518-10-07 00:45] falls asleep
[1518-10-30 00:25] wakes up
[1518-07-21 00:56] wakes up
[1518-08-18 00:53] falls asleep
[1518-05-22 00:48] wakes up
[1518-10-13 00:20] wakes up
[1518-08-08 00:04] Guard #331 begins shift
[1518-04-07 00:45] wakes up
[1518-07-23 00:46] falls asleep
[1518-03-12 00:49] wakes up
[1518-09-12 00:55] wakes up
[1518-09-10 00:45] wakes up
[1518-10-10 00:42] wakes up
[1518-08-26 00:20] falls asleep
[1518-08-15 00:02] Guard #659 begins shift
[1518-11-20 00:45] wakes up
[1518-02-27 00:17] falls asleep
[1518-08-28 23:57] Guard #2777 begins shift
[1518-10-08 00:17] falls asleep
[1518-11-15 00:47] wakes up
[1518-11-21 00:57] wakes up
[1518-03-26 00:22] wakes up
[1518-09-06 00:19] wakes up
[1518-11-03 00:39] falls asleep
[1518-03-25 00:33] falls asleep
[1518-08-04 23:59] Guard #157 begins shift
[1518-08-09 00:41] falls asleep
[1518-10-21 23:57] Guard #659 begins shift
[1518-06-19 00:56] wakes up
[1518-07-14 00:35] wakes up
[1518-09-25 00:01] falls asleep
[1518-03-01 00:28] falls asleep
[1518-06-06 00:31] falls asleep
[1518-03-16 00:52] falls asleep
[1518-04-10 00:18] falls asleep
[1518-03-09 23:58] Guard #571 begins shift
[1518-04-24 00:25] falls asleep
[1518-10-02 00:22] falls asleep
[1518-07-22 00:54] wakes up
[1518-03-31 00:58] wakes up
[1518-05-08 00:56] wakes up
[1518-11-17 00:14] wakes up
[1518-07-24 23:57] Guard #2729 begins shift
[1518-10-28 00:52] wakes up
[1518-07-09 00:02] falls asleep
[1518-08-26 00:09] wakes up
[1518-05-27 23:56] Guard #1823 begins shift
[1518-04-05 23:56] Guard #1481 begins shift
[1518-10-31 00:36] falls asleep
[1518-09-02 00:45] wakes up
[1518-06-21 00:07] falls asleep
[1518-09-24 00:02] Guard #1279 begins shift
[1518-03-24 23:58] Guard #1279 begins shift
[1518-09-05 00:07] falls asleep
[1518-07-09 00:53] wakes up
[1518-06-03 00:46] wakes up
[1518-10-13 00:45] wakes up
[1518-03-25 23:50] Guard #1877 begins shift
[1518-10-06 00:38] wakes up
[1518-11-06 23:59] Guard #3089 begins shift
[1518-03-13 00:24] falls asleep
[1518-03-05 00:44] falls asleep
[1518-05-07 23:47] Guard #1481 begins shift
[1518-02-27 00:51] wakes up
[1518-10-06 00:02] Guard #1171 begins shift
[1518-04-20 00:28] wakes up
[1518-08-24 00:52] wakes up
[1518-06-27 00:39] wakes up
[1518-10-01 00:28] falls asleep
[1518-11-14 23:58] Guard #3011 begins shift
[1518-10-05 00:39] falls asleep
[1518-03-18 00:55] wakes up
[1518-08-01 00:56] wakes up
[1518-05-25 00:38] falls asleep
[1518-05-07 00:34] wakes up
[1518-03-14 00:18] falls asleep
[1518-08-25 00:41] wakes up
[1518-05-16 00:00] Guard #2729 begins shift
[1518-08-19 00:03] Guard #2909 begins shift
[1518-10-17 00:40] falls asleep
[1518-09-08 00:38] falls asleep
[1518-04-30 00:59] wakes up
[1518-11-02 00:56] falls asleep
[1518-05-06 23:50] Guard #659 begins shift
[1518-03-22 00:00] Guard #1367 begins shift
[1518-09-22 00:04] falls asleep
[1518-07-12 00:40] wakes up
[1518-06-16 23:54] Guard #2729 begins shift
[1518-11-18 00:01] falls asleep
[1518-09-22 00:08] wakes up
[1518-08-19 00:50] falls asleep
[1518-04-17 00:47] wakes up
[1518-08-24 00:04] Guard #571 begins shift
[1518-05-23 00:42] falls asleep
[1518-03-06 00:58] wakes up
[1518-05-12 00:45] wakes up
[1518-07-22 00:00] falls asleep
[1518-07-02 00:44] wakes up
[1518-08-19 23:47] Guard #2777 begins shift
[1518-07-31 00:02] Guard #263 begins shift
[1518-09-19 00:46] falls asleep
[1518-07-15 23:54] Guard #1877 begins shift
[1518-03-03 00:46] falls asleep
[1518-08-27 00:43] wakes up
[1518-07-08 00:58] wakes up
[1518-05-16 23:59] Guard #263 begins shift
[1518-03-08 00:39] wakes up
[1518-06-02 00:47] falls asleep
[1518-07-06 00:01] Guard #571 begins shift
[1518-05-30 00:44] falls asleep
[1518-04-09 00:04] Guard #1279 begins shift
[1518-04-19 23:58] Guard #83 begins shift
[1518-08-08 00:54] falls asleep
[1518-09-30 00:00] Guard #2729 begins shift
[1518-06-13 23:50] Guard #1171 begins shift
[1518-05-08 00:55] falls asleep
[1518-06-29 00:37] wakes up
[1518-09-12 00:03] Guard #659 begins shift
[1518-10-23 00:55] wakes up
[1518-10-11 00:55] wakes up
[1518-11-21 00:56] falls asleep
[1518-11-05 00:44] wakes up
[1518-10-15 00:57] wakes up
[1518-05-23 00:00] Guard #3089 begins shift
[1518-11-17 00:33] wakes up
[1518-09-26 00:56] wakes up
[1518-08-02 00:01] Guard #157 begins shift
[1518-07-29 00:38] wakes up
[1518-10-22 00:59] wakes up
[1518-05-04 00:54] wakes up
[1518-09-18 00:54] wakes up
[1518-07-29 00:45] falls asleep
[1518-03-23 00:23] falls asleep
[1518-09-29 00:46] falls asleep
[1518-09-27 00:55] falls asleep
[1518-03-15 00:04] Guard #1877 begins shift
[1518-06-29 00:45] falls asleep
[1518-09-04 00:38] wakes up
[1518-08-16 00:54] falls asleep
[1518-06-09 00:02] Guard #2909 begins shift
[1518-04-12 00:16] falls asleep
[1518-10-10 23:57] Guard #2657 begins shift
[1518-04-08 00:00] Guard #197 begins shift
[1518-03-20 00:57] wakes up
[1518-09-15 00:26] falls asleep
[1518-06-08 00:38] falls asleep
[1518-08-16 00:57] wakes up
[1518-04-19 00:58] wakes up
[1518-05-29 00:05] falls asleep
[1518-06-27 00:29] falls asleep
[1518-03-01 00:56] wakes up
[1518-04-10 00:45] falls asleep
[1518-09-23 00:27] wakes up
[1518-07-25 00:24] wakes up
[1518-04-01 23:57] Guard #2657 begins shift
[1518-10-29 00:11] wakes up
[1518-08-28 00:34] falls asleep
[1518-06-17 00:45] falls asleep
[1518-04-24 00:58] wakes up
[1518-03-30 00:18] falls asleep
[1518-11-15 00:59] wakes up
[1518-10-24 00:24] falls asleep
[1518-08-02 00:35] falls asleep
[1518-06-19 00:04] Guard #197 begins shift
[1518-06-24 00:53] wakes up
[1518-07-15 00:51] wakes up
[1518-08-08 00:49] wakes up
[1518-11-19 00:33] wakes up
[1518-04-16 23:56] Guard #2729 begins shift
[1518-10-14 00:39] falls asleep
[1518-05-30 23:58] Guard #571 begins shift
[1518-05-26 00:52] wakes up
[1518-06-08 00:20] wakes up
[1518-06-08 00:09] falls asleep
[1518-04-26 00:56] wakes up
[1518-08-14 00:24] falls asleep
[1518-03-02 00:02] falls asleep
[1518-06-19 00:18] falls asleep
[1518-04-14 00:02] Guard #1279 begins shift
[1518-05-14 23:46] Guard #2777 begins shift
[1518-09-15 00:44] falls asleep
[1518-04-29 00:53] wakes up
[1518-04-16 00:24] falls asleep
[1518-05-16 00:41] wakes up
[1518-05-12 23:56] Guard #1877 begins shift
[1518-08-07 00:08] falls asleep
[1518-04-16 00:28] wakes up
[1518-09-11 00:46] wakes up
[1518-09-08 00:54] falls asleep
[1518-10-09 23:59] Guard #571 begins shift
[1518-06-16 00:03] Guard #659 begins shift
[1518-09-03 00:51] wakes up
[1518-08-03 00:02] Guard #3089 begins shift
[1518-06-09 23:56] Guard #3011 begins shift
[1518-03-05 00:55] wakes up
[1518-06-28 00:51] wakes up
[1518-10-21 00:45] falls asleep
[1518-11-02 23:53] Guard #1697 begins shift
[1518-10-02 00:23] wakes up
[1518-05-09 00:42] wakes up
[1518-04-23 00:08] falls asleep
[1518-03-05 00:49] wakes up
[1518-10-09 00:38] falls asleep
[1518-03-12 00:33] wakes up
[1518-05-11 00:11] falls asleep
[1518-04-15 00:36] falls asleep
[1518-11-10 00:58] wakes up
[1518-06-01 00:25] falls asleep
[1518-10-30 00:10] falls asleep
[1518-04-20 00:11] falls asleep
[1518-07-29 00:26] falls asleep
[1518-04-30 00:31] falls asleep
[1518-03-08 00:43] falls asleep
[1518-03-04 00:29] falls asleep
[1518-04-11 00:19] falls asleep
[1518-04-24 23:56] Guard #83 begins shift
[1518-04-16 00:55] wakes up
[1518-11-06 00:03] Guard #83 begins shift
[1518-03-18 00:09] falls asleep
[1518-03-10 00:57] falls asleep
[1518-10-20 23:50] Guard #1171 begins shift
[1518-09-30 00:48] wakes up
[1518-08-18 00:44] falls asleep
[1518-05-13 23:58] Guard #3011 begins shift
[1518-04-29 00:10] falls asleep
[1518-04-28 00:57] wakes up
[1518-05-21 00:13] falls asleep
[1518-06-27 00:46] falls asleep
[1518-08-12 00:38] wakes up
[1518-11-19 00:41] wakes up
[1518-04-11 00:02] Guard #2909 begins shift
[1518-06-29 00:52] wakes up
[1518-05-12 00:52] falls asleep
[1518-07-03 00:17] falls asleep
[1518-09-09 00:53] falls asleep
[1518-10-15 00:38] falls asleep
[1518-09-16 00:20] falls asleep
[1518-08-16 00:06] falls asleep
[1518-11-12 00:05] falls asleep
[1518-07-27 00:36] wakes up
[1518-03-20 00:34] wakes up
[1518-10-04 00:47] wakes up
[1518-05-04 00:43] wakes up
[1518-08-10 00:56] wakes up
[1518-08-03 00:51] wakes up
[1518-03-26 00:00] falls asleep
[1518-04-03 00:17] falls asleep
[1518-10-08 00:00] Guard #2657 begins shift
[1518-05-22 00:26] falls asleep
[1518-05-09 00:55] wakes up
[1518-11-22 00:02] Guard #157 begins shift
[1518-07-02 23:58] Guard #2909 begins shift
[1518-05-11 00:56] falls asleep
[1518-07-14 00:01] Guard #2909 begins shift
[1518-11-15 00:42] falls asleep
[1518-03-11 00:32] falls asleep
[1518-07-14 00:40] falls asleep
[1518-06-07 00:04] Guard #659 begins shift
[1518-06-05 00:00] falls asleep
[1518-05-06 00:19] falls asleep
[1518-06-04 00:00] Guard #2777 begins shift
[1518-05-30 00:47] wakes up
[1518-06-18 00:06] falls asleep
[1518-04-18 23:50] Guard #2729 begins shift
[1518-06-20 00:56] falls asleep
[1518-04-22 00:00] Guard #3011 begins shift
[1518-07-02 00:55] wakes up
[1518-09-01 00:09] falls asleep
[1518-09-27 00:01] Guard #1877 begins shift
[1518-11-20 00:55] falls asleep
[1518-09-19 00:52] wakes up
[1518-02-28 00:17] falls asleep
[1518-10-08 00:29] falls asleep
[1518-05-27 00:02] falls asleep
[1518-07-23 00:29] falls asleep
[1518-10-20 00:15] falls asleep
[1518-05-02 00:03] falls asleep
[1518-10-29 23:59] Guard #2657 begins shift
[1518-06-08 00:45] wakes up
[1518-11-04 00:18] falls asleep
[1518-09-06 00:41] falls asleep
[1518-07-22 00:47] wakes up
[1518-10-02 00:46] falls asleep
[1518-06-11 00:56] wakes up
[1518-05-08 00:52] wakes up
[1518-08-30 23:56] Guard #151 begins shift
[1518-08-12 23:46] Guard #263 begins shift
[1518-06-05 00:56] wakes up
[1518-05-10 00:39] wakes up
[1518-09-02 23:57] Guard #331 begins shift
[1518-07-07 00:44] wakes up
[1518-06-04 00:57] falls asleep
[1518-08-17 00:57] wakes up
[1518-06-27 00:04] Guard #83 begins shift
[1518-04-26 00:41] falls asleep
[1518-11-03 00:10] wakes up
[1518-07-21 00:04] Guard #571 begins shift
[1518-07-19 23:58] Guard #157 begins shift
[1518-11-19 23:57] Guard #1877 begins shift
[1518-09-13 00:20] falls asleep
[1518-11-13 00:42] wakes up
[1518-03-02 23:59] Guard #2729 begins shift
[1518-08-08 00:24] wakes up
[1518-10-24 00:17] falls asleep
[1518-05-04 23:51] Guard #2777 begins shift
[1518-05-17 00:57] wakes up
[1518-03-19 00:18] falls asleep
[1518-04-01 00:34] wakes up
[1518-10-13 00:08] falls asleep
[1518-04-18 00:00] Guard #571 begins shift
[1518-09-04 23:58] Guard #1823 begins shift
[1518-05-21 00:35] wakes up
[1518-07-22 00:51] falls asleep
[1518-06-15 00:03] Guard #2999 begins shift
[1518-09-03 23:59] Guard #197 begins shift
[1518-10-08 00:46] wakes up
[1518-11-07 23:59] Guard #3011 begins shift
[1518-08-23 00:18] falls asleep
[1518-10-22 00:46] falls asleep
[1518-06-18 00:24] wakes up
[1518-05-04 00:25] wakes up
[1518-11-06 00:54] wakes up
[1518-06-23 00:44] wakes up
[1518-07-22 00:40] falls asleep
[1518-04-13 00:57] wakes up
[1518-08-22 00:00] falls asleep
[1518-04-05 00:03] Guard #1367 begins shift
[1518-04-12 00:52] wakes up
[1518-07-28 00:57] wakes up
[1518-07-04 00:07] falls asleep
[1518-04-13 00:03] Guard #571 begins shift
[1518-11-23 00:46] falls asleep
[1518-10-31 00:49] wakes up
[1518-04-20 00:44] falls asleep
[1518-07-02 00:47] falls asleep
[1518-08-30 00:15] falls asleep
[1518-04-29 23:56] Guard #1481 begins shift
[1518-10-12 00:57] wakes up
[1518-08-15 00:51] wakes up
[1518-08-08 23:49] Guard #2909 begins shift
[1518-08-26 00:50] wakes up
[1518-08-13 00:41] wakes up
[1518-05-14 00:55] wakes up
[1518-05-30 00:01] Guard #1877 begins shift
[1518-08-01 00:24] wakes up
[1518-07-26 00:39] wakes up
[1518-03-01 00:00] falls asleep
[1518-06-23 00:00] Guard #1823 begins shift
[1518-03-21 00:38] falls asleep
[1518-09-04 00:26] falls asleep
[1518-08-15 00:16] falls asleep
[1518-03-05 00:53] falls asleep
[1518-10-12 00:50] falls asleep
[1518-08-14 00:58] wakes up
[1518-07-10 00:13] falls asleep
[1518-05-10 00:57] wakes up
[1518-08-13 00:49] falls asleep
[1518-03-29 00:03] falls asleep
[1518-09-24 23:51] Guard #3011 begins shift
[1518-09-15 23:56] Guard #197 begins shift
[1518-11-05 00:56] wakes up
[1518-10-04 00:04] Guard #157 begins shift
[1518-10-12 00:01] falls asleep
[1518-10-11 00:28] wakes up
[1518-03-23 23:59] Guard #1301 begins shift
[1518-05-04 00:33] falls asleep
[1518-10-11 00:26] falls asleep
[1518-08-28 00:00] Guard #1697 begins shift
[1518-06-09 00:07] falls asleep
[1518-10-22 23:57] Guard #1697 begins shift
[1518-10-21 00:40] wakes up
[1518-09-22 23:56] Guard #1481 begins shift
[1518-09-07 00:49] wakes up
[1518-05-06 00:53] wakes up
[1518-09-25 00:09] wakes up
[1518-11-13 00:02] Guard #1481 begins shift
[1518-11-18 00:28] wakes up
[1518-05-16 00:44] falls asleep
[1518-08-31 00:13] falls asleep
[1518-10-18 23:57] Guard #659 begins shift
[1518-05-07 00:21] falls asleep
[1518-11-04 00:52] wakes up
[1518-08-25 00:30] falls asleep
[1518-08-07 00:58] wakes up
[1518-03-01 23:50] Guard #2777 begins shift
[1518-04-03 00:57] falls asleep
[1518-06-30 00:38] wakes up
[1518-06-10 00:52] wakes up
[1518-09-20 00:07] falls asleep
[1518-11-11 00:29] wakes up
[1518-05-26 23:53] Guard #83 begins shift
[1518-09-05 00:42] wakes up
[1518-05-27 00:31] wakes up
[1518-06-11 00:26] falls asleep
[1518-09-10 00:04] falls asleep
[1518-06-24 00:04] Guard #1877 begins shift
[1518-06-22 00:53] wakes up
[1518-04-02 00:59] wakes up
[1518-03-25 00:53] wakes up
[1518-10-26 00:44] falls asleep
[1518-03-31 00:43] falls asleep
[1518-10-21 00:03] falls asleep
[1518-03-09 00:21] falls asleep
[1518-07-25 00:28] falls asleep
[1518-10-17 00:09] falls asleep
[1518-08-25 00:50] falls asleep
[1518-05-21 00:39] falls asleep
[1518-04-12 00:03] Guard #1279 begins shift
[1518-04-07 00:51] wakes up
[1518-10-19 00:42] falls asleep
[1518-06-10 23:57] Guard #2729 begins shift
[1518-09-29 00:01] Guard #1279 begins shift
[1518-10-20 00:00] Guard #3089 begins shift
[1518-05-17 00:23] falls asleep
[1518-06-20 23:57] Guard #2729 begins shift
[1518-04-22 00:47] falls asleep
[1518-07-27 23:54] Guard #2657 begins shift
[1518-04-16 00:33] falls asleep
[1518-08-08 00:58] wakes up
[1518-10-31 00:03] Guard #2777 begins shift
[1518-03-23 00:49] wakes up
[1518-05-31 00:53] wakes up
[1518-09-06 00:50] falls asleep
[1518-06-05 00:39] falls asleep
[1518-04-27 00:39] falls asleep
[1518-07-02 00:35] falls asleep
[1518-06-17 00:28] wakes up
[1518-07-25 00:54] wakes up
[1518-07-13 00:00] Guard #1279 begins shift
[1518-03-19 23:59] Guard #3011 begins shift
[1518-09-27 23:59] Guard #2657 begins shift
[1518-10-23 23:58] Guard #3011 begins shift
[1518-05-09 00:53] falls asleep
[1518-07-08 00:00] Guard #151 begins shift
[1518-03-12 00:00] falls asleep
[1518-08-11 00:01] Guard #2909 begins shift
[1518-03-10 00:19] falls asleep
[1518-03-01 00:15] wakes up
[1518-10-15 00:45] wakes up
[1518-07-03 00:53] wakes up
[1518-09-29 00:15] falls asleep
[1518-10-06 00:14] falls asleep
[1518-06-24 00:30] falls asleep
[1518-04-23 00:53] wakes up
[1518-11-17 00:12] falls asleep
[1518-08-16 00:03] Guard #571 begins shift
[1518-10-10 00:57] wakes up
[1518-06-01 23:58] Guard #659 begins shift
[1518-04-04 00:33] wakes up
[1518-04-15 00:00] Guard #3011 begins shift
[1518-04-17 00:12] falls asleep
[1518-10-27 00:54] wakes up
[1518-09-14 00:46] falls asleep
[1518-09-12 00:41] falls asleep
[1518-09-03 00:31] falls asleep
[1518-08-20 00:46] wakes up
[1518-05-03 00:03] Guard #659 begins shift
[1518-03-10 00:36] wakes up
[1518-04-20 00:52] wakes up
[1518-10-10 00:40] falls asleep
[1518-05-03 00:36] falls asleep
[1518-10-14 00:25] wakes up
[1518-04-11 00:32] wakes up
[1518-08-11 00:50] falls asleep
[1518-05-25 00:50] wakes up
[1518-11-01 00:53] wakes up
[1518-07-18 23:56] Guard #2999 begins shift
[1518-05-19 23:59] Guard #571 begins shift
[1518-04-02 00:12] falls asleep
[1518-05-15 00:38] wakes up
[1518-10-16 00:26] falls asleep
[1518-08-07 00:03] Guard #151 begins shift
[1518-10-27 23:56] Guard #659 begins shift
[1518-03-20 00:44] falls asleep
[1518-06-21 00:48] wakes up
[1518-10-29 00:36] wakes up
[1518-10-12 00:41] falls asleep
[1518-08-20 00:02] falls asleep
[1518-09-18 00:39] falls asleep
[1518-09-21 00:46] wakes up
[1518-07-03 00:41] falls asleep
[1518-07-10 00:26] falls asleep
[1518-07-18 00:20] falls asleep
[1518-11-02 00:31] falls asleep
[1518-02-28 00:00] Guard #263 begins shift
[1518-04-21 00:51] falls asleep
[1518-11-16 00:17] wakes up
[1518-03-26 00:57] falls asleep
[1518-09-23 00:39] wakes up
[1518-05-20 00:23] wakes up
[1518-11-22 00:49] wakes up
[1518-07-21 00:14] falls asleep
[1518-05-17 23:57] Guard #1823 begins shift
[1518-04-03 23:59] Guard #1171 begins shift
[1518-08-09 00:03] falls asleep
[1518-03-16 00:36] falls asleep
[1518-07-16 00:29] wakes up
[1518-11-10 23:53] Guard #151 begins shift
[1518-10-29 00:00] falls asleep
[1518-05-26 00:00] Guard #1171 begins shift
[1518-03-09 00:00] Guard #571 begins shift
[1518-05-23 00:39] wakes up
[1518-08-29 00:54] falls asleep
[1518-03-11 00:58] wakes up
[1518-08-27 00:12] falls asleep
[1518-07-28 23:59] Guard #1171 begins shift
[1518-10-11 00:35] falls asleep
[1518-08-11 00:57] wakes up
[1518-05-06 00:04] Guard #2909 begins shift
[1518-11-08 00:45] wakes up
[1518-08-10 00:38] falls asleep
[1518-10-17 23:58] Guard #1697 begins shift
[1518-06-25 23:56] Guard #157 begins shift
[1518-07-05 00:50] wakes up
[1518-07-21 00:45] wakes up
[1518-03-20 00:21] falls asleep
[1518-10-13 00:34] falls asleep
[1518-11-11 23:51] Guard #1823 begins shift
[1518-09-15 00:56] wakes up
[1518-07-23 00:58] wakes up
[1518-03-29 00:56] wakes up
[1518-07-15 00:23] wakes up
[1518-05-20 00:17] falls asleep
[1518-05-16 00:55] wakes up
[1518-04-10 00:39] wakes up
[1518-07-21 00:27] wakes up
[1518-10-29 00:55] wakes up
[1518-03-12 00:56] falls asleep
[1518-05-11 00:29] wakes up
[1518-06-17 00:01] falls asleep
[1518-11-03 00:05] falls asleep
[1518-07-22 23:57] Guard #157 begins shift
[1518-04-14 00:52] wakes up
[1518-08-08 00:12] falls asleep
[1518-07-29 00:56] wakes up
[1518-07-30 00:00] Guard #1171 begins shift
[1518-06-04 23:53] Guard #3011 begins shift
[1518-11-23 00:22] falls asleep
[1518-05-05 00:49] wakes up
[1518-04-25 00:52] wakes up
[1518-04-09 23:58] Guard #197 begins shift
[1518-09-06 00:44] wakes up
[1518-04-02 23:59] Guard #263 begins shift
[1518-10-16 00:53] wakes up
[1518-09-18 23:59] Guard #2657 begins shift
[1518-07-30 00:48] wakes up
[1518-06-24 23:56] Guard #3011 begins shift
[1518-09-22 00:49] falls asleep
[1518-09-02 00:52] falls asleep
[1518-03-16 23:54] Guard #331 begins shift
[1518-04-21 00:04] Guard #263 begins shift
[1518-06-25 00:47] wakes up
[1518-03-04 00:39] wakes up
[1518-08-14 00:44] falls asleep
[1518-05-01 23:46] Guard #1279 begins shift
[1518-08-01 00:54] falls asleep
[1518-09-19 00:10] falls asleep
[1518-07-07 00:03] Guard #3089 begins shift
[1518-03-09 00:33] wakes up
[1518-09-28 00:22] wakes up
[1518-08-09 00:54] wakes up
[1518-08-15 00:30] falls asleep
[1518-06-08 00:02] Guard #197 begins shift
[1518-03-03 00:54] wakes up
[1518-09-14 00:39] wakes up
[1518-10-25 00:55] wakes up
[1518-09-08 23:57] Guard #3011 begins shift
[1518-04-10 00:58] wakes up
[1518-04-14 00:22] wakes up
[1518-09-07 23:57] Guard #1279 begins shift
[1518-06-29 00:00] Guard #1279 begins shift
[1518-04-21 00:12] falls asleep
[1518-05-05 00:01] falls asleep
[1518-05-29 00:55] wakes up
[1518-11-14 00:38] falls asleep
[1518-11-23 00:33] wakes up
[1518-08-22 00:29] wakes up
[1518-09-19 00:43] wakes up
[1518-08-01 00:01] falls asleep
[1518-05-12 00:42] falls asleep
[1518-04-06 00:58] wakes up
[1518-04-15 00:21] falls asleep
[1518-03-29 00:54] falls asleep
[1518-08-13 00:01] falls asleep
[1518-05-25 00:00] Guard #1481 begins shift
[1518-11-01 00:04] Guard #1823 begins shift
[1518-10-09 00:04] Guard #3011 begins shift
[1518-10-17 00:28] wakes up
[1518-09-20 00:49] wakes up
[1518-03-11 23:53] Guard #1481 begins shift
[1518-10-11 23:46] Guard #1823 begins shift
[1518-09-09 00:48] wakes up
[1518-03-16 00:57] wakes up
[1518-05-10 00:47] falls asleep
[1518-09-06 23:57] Guard #331 begins shift
[1518-03-31 00:48] falls asleep
[1518-07-31 00:58] wakes up
[1518-07-05 00:02] Guard #1481 begins shift
[1518-11-09 23:57] Guard #2909 begins shift
[1518-05-24 00:57] wakes up
[1518-08-25 00:51] wakes up
[1518-11-21 00:15] falls asleep
[1518-06-20 00:47] wakes up
[1518-11-09 00:28] falls asleep
[1518-11-20 00:16] falls asleep
[1518-02-28 23:49] Guard #2909 begins shift
[1518-03-08 00:22] falls asleep
[1518-08-25 23:59] Guard #2729 begins shift
[1518-06-06 00:43] wakes up
[1518-11-01 00:20] falls asleep
[1518-07-21 00:53] falls asleep
[1518-05-20 00:35] falls asleep
[1518-07-07 00:18] falls asleep
[1518-08-11 00:40] wakes up
[1518-10-25 00:29] falls asleep
[1518-03-05 00:03] Guard #3011 begins shift
[1518-09-11 00:50] falls asleep
[1518-05-22 00:02] Guard #157 begins shift
[1518-03-17 00:39] wakes up
[1518-08-18 00:49] wakes up
[1518-08-22 23:59] Guard #331 begins shift
[1518-08-11 23:49] Guard #2777 begins shift
[1518-04-23 23:56] Guard #157 begins shift
[1518-05-11 00:57] wakes up
[1518-08-16 00:40] wakes up
[1518-05-08 23:58] Guard #1697 begins shift
[1518-08-04 00:01] falls asleep
[1518-03-21 00:00] Guard #157 begins shift
[1518-05-15 00:01] falls asleep
[1518-07-13 00:36] wakes up
[1518-08-05 00:58] wakes up
[1518-10-28 23:48] Guard #2777 begins shift
[1518-05-18 00:38] wakes up
[1518-03-31 23:59] Guard #263 begins shift
[1518-04-03 00:45] wakes up
[1518-07-30 00:22] falls asleep
[1518-06-16 00:31] wakes up
[1518-10-02 23:57] Guard #1279 begins shift
[1518-08-15 00:21] wakes up
[1518-07-10 23:58] Guard #2999 begins shift
[1518-09-26 00:04] falls asleep
[1518-04-30 23:59] Guard #157 begins shift
[1518-04-21 00:37] wakes up
[1518-10-14 00:51] wakes up
[1518-03-28 23:50] Guard #1481 begins shift
[1518-02-27 00:56] falls asleep
[1518-10-28 00:14] falls asleep
[1518-06-12 00:45] wakes up
[1518-11-08 00:40] falls asleep
[1518-07-12 00:00] Guard #157 begins shift
[1518-04-11 00:54] falls asleep
[1518-05-01 00:58] wakes up
[1518-09-15 00:03] Guard #1481 begins shift
[1518-06-30 23:57] Guard #151 begins shift
[1518-07-31 23:54] Guard #83 begins shift
[1518-07-03 00:24] wakes up
[1518-04-23 00:00] Guard #1481 begins shift
[1518-04-26 00:04] Guard #3011 begins shift
[1518-07-18 00:45] wakes up
[1518-03-19 00:52] wakes up
[1518-05-26 00:40] falls asleep
[1518-11-02 00:01] Guard #659 begins shift
[1518-09-27 00:36] wakes up
[1518-10-24 00:59] wakes up
[1518-06-20 00:46] falls asleep
[1518-03-31 00:45] wakes up
[1518-11-05 00:52] falls asleep
[1518-08-21 23:46] Guard #157 begins shift
[1518-06-14 00:51] wakes up
[1518-06-07 00:46] falls asleep
[1518-08-02 00:50] wakes up
[1518-10-07 00:53] wakes up
[1518-03-09 00:44] wakes up
[1518-07-09 00:34] wakes up
[1518-03-16 00:45] wakes up
[1518-05-11 23:59] Guard #197 begins shift
[1518-05-14 00:10] falls asleep
[1518-09-08 00:57] wakes up
[1518-09-21 00:04] Guard #1697 begins shift
[1518-03-29 00:46] wakes up
[1518-05-28 00:28] wakes up
[1518-09-30 23:56] Guard #1823 begins shift
[1518-06-13 00:04] Guard #2657 begins shift
[1518-11-03 00:51] wakes up
[1518-04-06 00:35] falls asleep
[1518-11-19 00:39] falls asleep
[1518-10-14 00:12] falls asleep
[1518-09-28 00:06] falls asleep
[1518-03-23 00:01] Guard #2657 begins shift
[1518-06-26 00:51] falls asleep
[1518-05-10 00:04] Guard #151 begins shift
[1518-08-07 00:29] wakes up
[1518-03-03 23:58] Guard #83 begins shift
[1518-09-17 00:52] wakes up
[1518-06-01 00:52] wakes up
[1518-08-23 00:50] wakes up
[1518-08-13 00:58] wakes up
[1518-08-06 00:48] wakes up
[1518-09-23 00:50] falls asleep
[1518-10-03 00:39] wakes up
[1518-03-27 00:51] wakes up
[1518-04-27 00:04] Guard #2909 begins shift
[1518-09-02 00:09] falls asleep
[1518-04-04 00:30] falls asleep
[1518-05-07 00:41] falls asleep
[1518-03-12 00:42] falls asleep
[1518-05-18 00:44] falls asleep
[1518-04-14 00:16] falls asleep
[1518-06-12 00:31] falls asleep
[1518-10-09 00:55] wakes up
[1518-11-23 00:51] wakes up
[1518-09-11 00:56] wakes up
[1518-09-20 00:01] Guard #2657 begins shift
[1518-05-23 00:32] falls asleep
[1518-11-19 00:30] falls asleep
[1518-10-19 00:49] wakes up
[1518-09-30 00:41] falls asleep
[1518-09-17 00:12] falls asleep
[1518-03-28 00:46] falls asleep
[1518-07-02 00:04] Guard #331 begins shift
[1518-04-14 00:27] falls asleep
[1518-10-15 00:04] Guard #2777 begins shift
[1518-11-20 00:56] wakes up
[1518-04-15 23:59] Guard #571 begins shift
[1518-04-01 00:19] falls asleep
[1518-06-25 00:26] falls asleep
[1518-05-28 00:08] falls asleep
[1518-03-31 00:01] Guard #2657 begins shift
[1518-04-28 00:41] falls asleep
[1518-06-01 00:04] Guard #1481 begins shift
[1518-05-18 00:48] wakes up
[1518-06-29 23:48] Guard #151 begins shift
[1518-03-11 00:01] Guard #1171 begins shift
[1518-07-28 00:05] falls asleep
[1518-03-14 00:46] wakes up
[1518-07-26 00:27] falls asleep
[1518-05-24 00:47] falls asleep
[1518-05-17 00:36] wakes up
[1518-06-21 00:42] wakes up
[1518-09-11 00:41] falls asleep
[1518-06-09 00:54] wakes up
[1518-06-13 00:49] wakes up
[1518-11-15 23:57] Guard #2909 begins shift
[1518-06-22 00:35] falls asleep
[1518-04-11 00:55] wakes up
[1518-04-18 00:28] falls asleep
[1518-03-08 00:54] wakes up
[1518-11-08 23:59] Guard #659 begins shift
[1518-05-02 00:32] wakes up
[1518-11-15 00:57] falls asleep
[1518-04-09 00:28] falls asleep
[1518-11-04 23:56] Guard #331 begins shift
[1518-09-27 00:46] wakes up
[1518-10-16 00:04] Guard #3089 begins shift
[1518-05-31 00:33] falls asleep
[1518-10-07 00:02] Guard #331 begins shift
[1518-03-27 00:03] Guard #1823 begins shift
[1518-06-07 00:58] wakes up
[1518-08-27 00:03] Guard #2729 begins shift
[1518-03-08 00:01] Guard #331 begins shift
[1518-07-08 00:27] falls asleep
[1518-10-21 00:54] wakes up
[1518-11-13 23:59] Guard #2729 begins shift
[1518-03-18 00:21] wakes up
[1518-03-09 00:39] falls asleep
[1518-04-03 00:58] wakes up
[1518-11-16 23:59] Guard #151 begins shift
[1518-03-30 00:46] wakes up
[1518-06-22 00:01] Guard #197 begins shift
[1518-08-06 00:28] falls asleep
[1518-05-09 00:36] falls asleep
[1518-06-18 00:00] Guard #1877 begins shift
[1518-10-12 00:43] wakes up
[1518-10-15 00:50] falls asleep
[1518-05-13 00:57] wakes up
[1518-05-04 00:48] falls asleep
[1518-09-15 00:27] wakes up
[1518-07-14 00:59] wakes up
[1518-08-01 00:34] falls asleep
[1518-09-08 00:26] falls asleep
[1518-05-28 23:51] Guard #2909 begins shift
[1518-10-01 23:58] Guard #331 begins shift
[1518-08-17 00:02] Guard #197 begins shift
[1518-05-26 00:25] falls asleep
[1518-07-08 23:49] Guard #3011 begins shift
[1518-11-16 00:14] falls asleep
[1518-06-11 23:57] Guard #3089 begins shift
[1518-08-04 00:43] wakes up
[1518-09-27 00:44] falls asleep
[1518-05-12 00:53] wakes up
[1518-06-06 00:01] Guard #157 begins shift
[1518-03-06 00:49] wakes up
[1518-08-01 00:42] wakes up
[1518-05-03 23:58] Guard #197 begins shift
[1518-09-17 00:03] Guard #3011 begins shift
[1518-11-17 00:20] falls asleep
[1518-11-07 00:54] wakes up
[1518-11-06 00:15] falls asleep
[1518-06-20 00:03] Guard #1697 begins shift
[1518-10-21 00:46] wakes up
[1518-08-17 23:58] Guard #1697 begins shift
[1518-03-27 00:21] falls asleep
[1518-04-08 00:36] falls asleep
[1518-03-05 23:56] Guard #2729 begins shift
[1518-07-05 00:21] falls asleep
[1518-07-09 23:58] Guard #3011 begins shift
[1518-06-02 00:54] falls asleep
[1518-09-27 00:15] falls asleep
[1518-10-05 00:00] Guard #659 begins shift
[1518-07-09 00:41] falls asleep
[1518-05-23 00:59] wakes up
[1518-10-14 00:03] Guard #151 begins shift
[1518-11-05 00:19] falls asleep
[1518-06-04 00:59] wakes up
[1518-09-09 23:50] Guard #1823 begins shift
[1518-09-08 00:30] wakes up
[1518-08-07 00:53] falls asleep
[1518-10-24 23:56] Guard #1171 begins shift
[1518-08-09 23:57] Guard #2777 begins shift
[1518-03-15 00:14] wakes up
[1518-10-30 00:31] falls asleep
[1518-11-19 00:55] wakes up
[1518-08-08 00:32] falls asleep
[1518-02-26 23:59] Guard #571 begins shift
[1518-03-13 00:49] wakes up
[1518-04-06 00:57] falls asleep
[1518-10-26 00:52] wakes up
[1518-08-29 00:42] falls asleep
[1518-07-15 00:38] falls asleep
[1518-09-21 23:51] Guard #157 begins shift
[1518-09-05 23:52] Guard #2909 begins shift
[1518-10-27 00:03] Guard #1877 begins shift
[1518-08-18 00:58] wakes up
[1518-05-04 00:11] falls asleep
[1518-07-15 00:09] falls asleep
[1518-03-02 00:49] wakes up
[1518-08-24 00:16] falls asleep
[1518-10-12 00:17] wakes up
[1518-06-02 00:49] wakes up
[1518-07-24 00:04] Guard #1367 begins shift
[1518-08-11 00:37] falls asleep
[1518-03-17 00:05] falls asleep
[1518-03-21 00:58] wakes up
[1518-05-20 00:46] wakes up
[1518-07-14 00:30] falls asleep
[1518-07-22 00:31] wakes up
[1518-11-10 00:36] falls asleep
[1518-05-08 00:04] falls asleep
[1518-11-23 00:02] Guard #2657 begins shift
[1518-04-18 00:45] wakes up
[1518-04-22 00:48] wakes up
[1518-08-21 00:01] Guard #1301 begins shift
[1518-07-21 00:36] falls asleep
[1518-10-21 00:52] falls asleep
[1518-04-08 00:59] wakes up
[1518-09-18 00:34] wakes up
[1518-10-24 00:19] wakes up
[1518-08-30 00:32] wakes up
[1518-09-08 00:40] wakes up
[1518-06-10 00:13] falls asleep
[1518-05-16 00:25] falls asleep
"
           ))

(def day3 (str/split-lines
           "#1 @ 861,330: 20x10
#2 @ 491,428: 28x23
#3 @ 64,746: 20x27
#4 @ 406,769: 25x28
#5 @ 853,621: 17x26
#6 @ 311,802: 27x28
#7 @ 947,977: 14x13
#8 @ 786,5: 18x23
#9 @ 420,429: 14x24
#10 @ 138,206: 29x28
#11 @ 474,698: 19x29
#12 @ 525,141: 16x29
#13 @ 18,738: 11x29
#14 @ 166,402: 26x12
#15 @ 177,580: 18x14
#16 @ 105,564: 17x21
#17 @ 336,772: 12x21
#18 @ 229,923: 29x22
#19 @ 519,5: 26x14
#20 @ 245,608: 15x25
#21 @ 140,235: 11x20
#22 @ 571,382: 18x29
#23 @ 646,5: 24x29
#24 @ 159,677: 21x10
#25 @ 917,962: 20x29
#26 @ 692,896: 29x10
#27 @ 744,92: 12x21
#28 @ 969,390: 17x21
#29 @ 232,786: 27x28
#30 @ 415,568: 11x23
#31 @ 364,525: 14x21
#32 @ 936,560: 12x21
#33 @ 306,700: 25x15
#34 @ 902,356: 26x11
#35 @ 395,598: 24x17
#36 @ 691,977: 18x16
#37 @ 489,458: 21x27
#38 @ 10,58: 17x10
#39 @ 466,850: 14x27
#40 @ 640,152: 23x19
#41 @ 510,769: 17x21
#42 @ 695,332: 11x27
#43 @ 230,319: 12x15
#44 @ 356,435: 20x29
#45 @ 965,643: 14x21
#46 @ 221,909: 11x11
#47 @ 178,947: 11x27
#48 @ 9,508: 19x16
#49 @ 763,28: 28x15
#50 @ 154,159: 20x24
#51 @ 797,515: 11x19
#52 @ 670,393: 19x25
#53 @ 401,838: 17x16
#54 @ 518,153: 11x22
#55 @ 378,38: 18x27
#56 @ 759,548: 26x26
#57 @ 535,388: 16x20
#58 @ 331,502: 27x24
#59 @ 632,627: 21x16
#60 @ 469,351: 11x10
#61 @ 311,115: 21x12
#62 @ 373,80: 13x23
#63 @ 392,223: 24x17
#64 @ 937,408: 10x24
#65 @ 835,123: 12x11
#66 @ 539,420: 26x27
#67 @ 250,486: 17x25
#68 @ 130,25: 13x15
#69 @ 959,802: 19x20
#70 @ 184,871: 22x23
#71 @ 585,970: 12x15
#72 @ 800,251: 25x27
#73 @ 952,981: 18x12
#74 @ 274,96: 23x25
#75 @ 808,666: 11x11
#76 @ 727,795: 24x18
#77 @ 617,624: 16x27
#78 @ 588,398: 26x17
#79 @ 379,180: 21x20
#80 @ 875,58: 12x14
#81 @ 886,166: 20x10
#82 @ 164,670: 17x11
#83 @ 248,510: 20x18
#84 @ 518,548: 29x28
#85 @ 895,666: 16x22
#86 @ 80,651: 17x16
#87 @ 483,565: 23x26
#88 @ 223,141: 4x13
#89 @ 696,704: 13x11
#90 @ 237,965: 10x12
#91 @ 196,45: 14x17
#92 @ 520,245: 19x10
#93 @ 802,441: 15x12
#94 @ 919,354: 16x22
#95 @ 722,836: 16x15
#96 @ 493,863: 9x7
#97 @ 398,666: 11x27
#98 @ 608,708: 28x27
#99 @ 130,39: 17x17
#100 @ 487,128: 22x19
#101 @ 277,315: 3x15
#102 @ 883,612: 29x19
#103 @ 646,750: 20x29
#104 @ 512,465: 11x12
#105 @ 333,818: 22x26
#106 @ 22,355: 21x15
#107 @ 180,95: 12x10
#108 @ 795,278: 27x18
#109 @ 533,139: 23x18
#110 @ 507,445: 17x18
#111 @ 763,147: 19x15
#112 @ 687,749: 17x13
#113 @ 597,691: 12x19
#114 @ 509,44: 11x27
#115 @ 706,38: 10x20
#116 @ 112,91: 28x14
#117 @ 825,416: 14x26
#118 @ 850,330: 27x13
#119 @ 576,5: 26x20
#120 @ 778,75: 29x18
#121 @ 847,127: 26x29
#122 @ 265,695: 17x26
#123 @ 828,676: 21x20
#124 @ 933,268: 22x28
#125 @ 74,746: 29x17
#126 @ 511,66: 29x28
#127 @ 960,150: 25x13
#128 @ 891,810: 10x28
#129 @ 887,899: 13x15
#130 @ 648,413: 14x20
#131 @ 113,882: 20x16
#132 @ 532,171: 10x13
#133 @ 91,427: 29x12
#134 @ 86,333: 27x15
#135 @ 558,725: 29x14
#136 @ 574,504: 19x15
#137 @ 219,566: 10x11
#138 @ 432,333: 9x7
#139 @ 952,591: 6x15
#140 @ 894,535: 22x15
#141 @ 885,345: 21x29
#142 @ 422,416: 20x24
#143 @ 620,642: 22x26
#144 @ 115,459: 14x28
#145 @ 371,866: 11x14
#146 @ 677,594: 25x17
#147 @ 607,638: 26x20
#148 @ 232,87: 21x16
#149 @ 738,668: 12x19
#150 @ 582,782: 28x11
#151 @ 17,600: 11x24
#152 @ 519,126: 27x23
#153 @ 430,331: 16x13
#154 @ 197,656: 13x21
#155 @ 480,315: 13x20
#156 @ 750,80: 12x28
#157 @ 590,924: 24x29
#158 @ 699,984: 28x11
#159 @ 385,488: 20x25
#160 @ 774,289: 6x19
#161 @ 197,913: 21x14
#162 @ 650,91: 20x22
#163 @ 675,25: 25x25
#164 @ 159,507: 18x10
#165 @ 872,60: 20x25
#166 @ 897,566: 23x25
#167 @ 73,242: 15x14
#168 @ 57,785: 12x13
#169 @ 99,902: 24x22
#170 @ 840,959: 25x13
#171 @ 267,965: 28x14
#172 @ 464,367: 20x11
#173 @ 945,71: 21x18
#174 @ 792,918: 28x29
#175 @ 658,940: 24x13
#176 @ 284,415: 19x11
#177 @ 107,221: 23x14
#178 @ 266,595: 28x16
#179 @ 531,599: 25x28
#180 @ 938,97: 20x15
#181 @ 246,371: 16x16
#182 @ 922,772: 21x12
#183 @ 260,911: 18x10
#184 @ 964,629: 24x18
#185 @ 773,879: 13x16
#186 @ 569,803: 17x28
#187 @ 836,615: 24x19
#188 @ 970,623: 24x15
#189 @ 828,216: 19x17
#190 @ 267,178: 22x18
#191 @ 302,121: 14x15
#192 @ 156,165: 12x11
#193 @ 352,515: 27x28
#194 @ 151,510: 12x13
#195 @ 645,335: 25x19
#196 @ 818,872: 10x12
#197 @ 460,879: 3x8
#198 @ 222,391: 14x20
#199 @ 127,139: 27x13
#200 @ 676,920: 22x22
#201 @ 467,479: 12x27
#202 @ 62,608: 25x26
#203 @ 843,80: 15x17
#204 @ 258,600: 26x13
#205 @ 2,345: 8x4
#206 @ 181,885: 23x11
#207 @ 765,656: 11x14
#208 @ 528,919: 28x15
#209 @ 311,774: 27x20
#210 @ 343,683: 27x18
#211 @ 395,898: 13x23
#212 @ 322,555: 12x12
#213 @ 764,661: 16x11
#214 @ 274,363: 24x16
#215 @ 845,977: 7x16
#216 @ 656,641: 13x28
#217 @ 805,228: 26x11
#218 @ 51,113: 24x14
#219 @ 777,658: 28x21
#220 @ 185,915: 14x15
#221 @ 296,794: 24x17
#222 @ 507,468: 17x21
#223 @ 708,337: 23x11
#224 @ 522,154: 13x27
#225 @ 382,762: 22x29
#226 @ 635,49: 21x22
#227 @ 646,265: 18x23
#228 @ 369,562: 28x22
#229 @ 420,570: 26x19
#230 @ 487,861: 24x12
#231 @ 535,29: 29x14
#232 @ 89,816: 20x27
#233 @ 835,51: 19x24
#234 @ 395,473: 11x15
#235 @ 620,137: 21x27
#236 @ 314,37: 29x20
#237 @ 61,426: 27x23
#238 @ 178,356: 21x26
#239 @ 95,722: 16x10
#240 @ 744,879: 11x24
#241 @ 662,878: 25x13
#242 @ 359,156: 21x29
#243 @ 632,620: 29x10
#244 @ 746,676: 11x20
#245 @ 474,284: 19x13
#246 @ 778,506: 27x13
#247 @ 718,337: 10x17
#248 @ 52,360: 11x20
#249 @ 243,492: 12x22
#250 @ 958,139: 19x23
#251 @ 810,105: 29x23
#252 @ 727,897: 11x22
#253 @ 56,33: 26x19
#254 @ 198,945: 15x23
#255 @ 929,319: 22x20
#256 @ 393,362: 14x22
#257 @ 273,27: 12x23
#258 @ 668,857: 24x27
#259 @ 956,681: 14x13
#260 @ 685,317: 22x21
#261 @ 562,410: 21x17
#262 @ 605,630: 14x23
#263 @ 946,393: 26x17
#264 @ 98,225: 14x26
#265 @ 0,3: 23x20
#266 @ 162,949: 27x20
#267 @ 829,548: 18x13
#268 @ 842,64: 18x17
#269 @ 80,47: 15x16
#270 @ 116,613: 22x24
#271 @ 9,492: 26x26
#272 @ 29,129: 21x25
#273 @ 86,246: 15x26
#274 @ 360,164: 23x15
#275 @ 528,530: 18x17
#276 @ 796,668: 19x22
#277 @ 827,178: 25x29
#278 @ 873,714: 27x12
#279 @ 543,276: 18x16
#280 @ 641,619: 24x10
#281 @ 658,773: 11x21
#282 @ 477,442: 13x27
#283 @ 864,791: 15x20
#284 @ 508,436: 27x14
#285 @ 908,348: 19x19
#286 @ 893,813: 5x20
#287 @ 801,558: 19x23
#288 @ 170,324: 12x15
#289 @ 151,355: 14x8
#290 @ 247,771: 12x25
#291 @ 660,862: 11x24
#292 @ 192,704: 13x28
#293 @ 370,734: 11x18
#294 @ 687,877: 21x10
#295 @ 74,263: 23x22
#296 @ 609,524: 26x24
#297 @ 127,723: 20x12
#298 @ 468,124: 26x14
#299 @ 153,281: 17x10
#300 @ 831,261: 11x27
#301 @ 64,282: 25x27
#302 @ 873,364: 11x26
#303 @ 707,305: 15x22
#304 @ 746,626: 22x20
#305 @ 718,590: 22x12
#306 @ 593,745: 17x13
#307 @ 416,297: 15x27
#308 @ 82,609: 18x17
#309 @ 468,885: 12x21
#310 @ 31,341: 24x24
#311 @ 585,541: 29x25
#312 @ 882,713: 13x14
#313 @ 499,420: 19x27
#314 @ 108,125: 11x26
#315 @ 248,709: 3x9
#316 @ 117,122: 22x28
#317 @ 965,800: 24x21
#318 @ 757,567: 17x28
#319 @ 193,9: 17x12
#320 @ 89,240: 12x19
#321 @ 29,726: 13x21
#322 @ 169,826: 11x17
#323 @ 475,499: 28x21
#324 @ 187,323: 12x12
#325 @ 319,694: 20x22
#326 @ 91,6: 17x16
#327 @ 79,174: 28x21
#328 @ 872,139: 24x21
#329 @ 74,330: 27x11
#330 @ 813,965: 29x29
#331 @ 588,442: 20x10
#332 @ 901,674: 23x16
#333 @ 14,248: 29x10
#334 @ 512,251: 10x13
#335 @ 409,589: 12x29
#336 @ 799,537: 18x22
#337 @ 230,537: 16x12
#338 @ 491,275: 25x18
#339 @ 829,183: 19x11
#340 @ 233,327: 12x27
#341 @ 844,803: 21x15
#342 @ 558,115: 19x13
#343 @ 521,826: 15x29
#344 @ 152,21: 18x29
#345 @ 159,37: 14x15
#346 @ 390,266: 11x12
#347 @ 297,724: 23x19
#348 @ 592,318: 11x17
#349 @ 186,923: 14x13
#350 @ 471,798: 10x25
#351 @ 30,192: 25x19
#352 @ 532,830: 10x27
#353 @ 71,964: 14x27
#354 @ 551,7: 28x27
#355 @ 117,135: 11x23
#356 @ 240,367: 12x21
#357 @ 599,700: 8x10
#358 @ 182,222: 17x24
#359 @ 449,357: 24x24
#360 @ 92,521: 26x21
#361 @ 845,214: 26x22
#362 @ 188,647: 13x29
#363 @ 440,24: 12x28
#364 @ 855,957: 23x27
#365 @ 229,897: 28x25
#366 @ 700,971: 20x16
#367 @ 798,651: 14x12
#368 @ 889,379: 13x24
#369 @ 498,787: 13x12
#370 @ 584,810: 16x15
#371 @ 8,498: 14x19
#372 @ 802,467: 21x18
#373 @ 418,399: 12x20
#374 @ 300,78: 22x15
#375 @ 396,917: 15x28
#376 @ 159,149: 16x25
#377 @ 37,368: 19x19
#378 @ 301,599: 12x18
#379 @ 83,260: 28x14
#380 @ 563,884: 14x22
#381 @ 777,189: 21x18
#382 @ 400,392: 18x15
#383 @ 168,959: 19x17
#384 @ 184,952: 11x23
#385 @ 796,628: 24x27
#386 @ 493,31: 25x11
#387 @ 136,129: 27x27
#388 @ 714,693: 6x21
#389 @ 557,414: 28x11
#390 @ 733,601: 28x18
#391 @ 344,536: 22x25
#392 @ 782,831: 19x15
#393 @ 86,260: 18x20
#394 @ 435,617: 14x15
#395 @ 78,951: 20x28
#396 @ 207,138: 26x20
#397 @ 172,38: 17x17
#398 @ 158,886: 15x21
#399 @ 929,563: 26x12
#400 @ 350,562: 19x27
#401 @ 187,19: 22x12
#402 @ 23,320: 28x15
#403 @ 309,54: 11x27
#404 @ 880,35: 25x19
#405 @ 849,58: 29x28
#406 @ 912,679: 6x5
#407 @ 785,795: 22x27
#408 @ 386,681: 29x28
#409 @ 912,579: 21x29
#410 @ 749,772: 28x13
#411 @ 362,175: 21x11
#412 @ 265,916: 16x16
#413 @ 863,834: 18x26
#414 @ 946,64: 18x12
#415 @ 252,552: 27x28
#416 @ 952,981: 21x12
#417 @ 506,11: 16x13
#418 @ 794,363: 27x11
#419 @ 259,886: 19x20
#420 @ 205,879: 23x14
#421 @ 101,907: 12x22
#422 @ 366,736: 24x18
#423 @ 692,50: 25x29
#424 @ 919,465: 23x10
#425 @ 73,235: 10x18
#426 @ 589,664: 27x13
#427 @ 255,376: 13x12
#428 @ 804,129: 16x12
#429 @ 450,340: 25x12
#430 @ 754,161: 16x20
#431 @ 368,708: 12x12
#432 @ 504,365: 27x26
#433 @ 673,737: 26x10
#434 @ 462,358: 14x29
#435 @ 901,569: 7x9
#436 @ 932,103: 15x21
#437 @ 262,844: 11x19
#438 @ 367,400: 25x22
#439 @ 249,247: 17x28
#440 @ 451,67: 3x20
#441 @ 757,152: 14x21
#442 @ 852,799: 12x10
#443 @ 554,93: 10x18
#444 @ 82,420: 13x13
#445 @ 401,843: 18x27
#446 @ 454,184: 24x19
#447 @ 681,253: 16x10
#448 @ 942,247: 22x17
#449 @ 774,784: 28x14
#450 @ 273,94: 26x19
#451 @ 230,782: 19x20
#452 @ 517,396: 18x15
#453 @ 471,485: 17x21
#454 @ 383,926: 23x11
#455 @ 481,687: 10x29
#456 @ 39,171: 18x11
#457 @ 285,489: 28x10
#458 @ 0,341: 14x15
#459 @ 268,206: 14x26
#460 @ 459,9: 28x29
#461 @ 244,695: 11x28
#462 @ 851,227: 20x13
#463 @ 241,132: 15x12
#464 @ 287,65: 26x25
#465 @ 328,700: 16x22
#466 @ 428,380: 23x27
#467 @ 695,17: 20x13
#468 @ 193,187: 21x22
#469 @ 98,495: 24x16
#470 @ 720,816: 14x25
#471 @ 193,400: 28x29
#472 @ 977,136: 21x27
#473 @ 645,858: 16x23
#474 @ 785,416: 17x20
#475 @ 240,374: 29x25
#476 @ 9,711: 27x19
#477 @ 823,273: 28x28
#478 @ 153,538: 12x3
#479 @ 286,354: 20x17
#480 @ 795,657: 21x29
#481 @ 483,17: 25x10
#482 @ 87,881: 13x15
#483 @ 881,897: 14x10
#484 @ 404,92: 29x24
#485 @ 946,630: 22x17
#486 @ 167,257: 20x17
#487 @ 58,483: 24x16
#488 @ 632,39: 29x23
#489 @ 762,248: 19x21
#490 @ 599,443: 23x14
#491 @ 455,18: 26x14
#492 @ 458,817: 27x19
#493 @ 868,685: 11x3
#494 @ 190,969: 10x29
#495 @ 764,460: 24x22
#496 @ 894,70: 17x16
#497 @ 541,880: 29x16
#498 @ 307,837: 13x26
#499 @ 158,943: 13x29
#500 @ 447,974: 11x11
#501 @ 845,881: 23x28
#502 @ 840,76: 11x25
#503 @ 785,11: 25x16
#504 @ 933,625: 18x24
#505 @ 919,941: 22x12
#506 @ 812,442: 14x18
#507 @ 945,834: 21x27
#508 @ 711,738: 10x20
#509 @ 269,893: 17x25
#510 @ 796,408: 19x24
#511 @ 83,146: 26x21
#512 @ 611,782: 14x18
#513 @ 222,168: 23x12
#514 @ 190,211: 13x14
#515 @ 455,890: 14x17
#516 @ 344,350: 15x27
#517 @ 322,975: 16x12
#518 @ 96,665: 15x14
#519 @ 746,936: 29x14
#520 @ 636,756: 25x28
#521 @ 769,73: 12x28
#522 @ 385,591: 24x17
#523 @ 642,348: 19x19
#524 @ 696,451: 16x26
#525 @ 171,319: 27x29
#526 @ 912,849: 26x21
#527 @ 25,50: 25x25
#528 @ 954,927: 19x18
#529 @ 856,151: 19x25
#530 @ 168,364: 14x17
#531 @ 760,588: 12x25
#532 @ 217,169: 17x28
#533 @ 241,80: 15x17
#534 @ 179,388: 18x21
#535 @ 922,732: 28x27
#536 @ 637,368: 22x28
#537 @ 812,563: 13x13
#538 @ 291,311: 29x19
#539 @ 260,671: 14x25
#540 @ 764,141: 19x16
#541 @ 847,223: 16x8
#542 @ 862,283: 26x29
#543 @ 962,624: 10x14
#544 @ 450,7: 21x18
#545 @ 469,762: 22x26
#546 @ 786,611: 26x15
#547 @ 282,85: 20x17
#548 @ 952,902: 19x29
#549 @ 68,556: 19x23
#550 @ 650,82: 22x20
#551 @ 870,296: 10x26
#552 @ 490,24: 26x23
#553 @ 684,253: 24x18
#554 @ 606,397: 15x26
#555 @ 8,54: 28x23
#556 @ 675,336: 21x14
#557 @ 378,571: 13x26
#558 @ 64,39: 20x15
#559 @ 60,838: 22x17
#560 @ 401,114: 24x27
#561 @ 526,33: 23x21
#562 @ 668,534: 11x26
#563 @ 370,862: 26x24
#564 @ 39,188: 28x10
#565 @ 370,60: 14x13
#566 @ 448,544: 13x20
#567 @ 320,919: 19x13
#568 @ 212,913: 29x22
#569 @ 951,972: 14x15
#570 @ 146,628: 12x21
#571 @ 696,792: 29x15
#572 @ 509,508: 18x23
#573 @ 535,320: 21x11
#574 @ 448,202: 15x23
#575 @ 483,905: 23x13
#576 @ 30,799: 18x21
#577 @ 499,1: 13x23
#578 @ 654,626: 19x27
#579 @ 293,975: 22x15
#580 @ 400,463: 10x10
#581 @ 55,633: 23x25
#582 @ 770,200: 11x14
#583 @ 96,481: 23x21
#584 @ 651,685: 21x11
#585 @ 508,394: 24x13
#586 @ 83,273: 16x11
#587 @ 94,541: 27x20
#588 @ 700,181: 19x12
#589 @ 980,22: 16x24
#590 @ 884,473: 20x25
#591 @ 346,613: 25x11
#592 @ 81,604: 24x13
#593 @ 677,342: 26x28
#594 @ 750,786: 16x14
#595 @ 333,972: 10x14
#596 @ 976,27: 16x20
#597 @ 842,974: 16x25
#598 @ 178,328: 22x21
#599 @ 401,979: 14x18
#600 @ 656,788: 27x14
#601 @ 692,320: 21x23
#602 @ 741,634: 22x25
#603 @ 341,711: 25x28
#604 @ 420,373: 28x14
#605 @ 250,827: 22x18
#606 @ 588,513: 29x14
#607 @ 902,94: 11x14
#608 @ 418,828: 27x19
#609 @ 1,471: 19x23
#610 @ 460,743: 15x10
#611 @ 213,864: 27x18
#612 @ 127,237: 14x21
#613 @ 61,700: 22x12
#614 @ 864,308: 20x11
#615 @ 213,264: 14x17
#616 @ 264,49: 17x29
#617 @ 391,764: 16x12
#618 @ 733,822: 14x24
#619 @ 265,907: 13x15
#620 @ 612,947: 15x21
#621 @ 916,208: 27x24
#622 @ 350,798: 13x14
#623 @ 175,168: 21x29
#624 @ 556,886: 11x15
#625 @ 305,827: 25x19
#626 @ 699,793: 25x24
#627 @ 983,202: 17x28
#628 @ 902,83: 15x18
#629 @ 209,921: 29x11
#630 @ 154,611: 26x17
#631 @ 762,975: 11x13
#632 @ 476,354: 13x19
#633 @ 298,401: 26x28
#634 @ 471,896: 13x11
#635 @ 853,382: 15x22
#636 @ 826,690: 26x21
#637 @ 463,313: 18x23
#638 @ 590,550: 22x23
#639 @ 309,904: 18x11
#640 @ 912,593: 22x25
#641 @ 781,185: 12x15
#642 @ 103,396: 16x11
#643 @ 849,128: 17x11
#644 @ 82,237: 25x15
#645 @ 240,2: 21x11
#646 @ 621,541: 10x21
#647 @ 918,363: 10x29
#648 @ 248,138: 15x17
#649 @ 77,944: 23x26
#650 @ 544,116: 17x17
#651 @ 442,806: 16x27
#652 @ 618,353: 25x14
#653 @ 138,658: 22x22
#654 @ 71,832: 18x13
#655 @ 947,541: 23x21
#656 @ 114,494: 21x12
#657 @ 664,331: 12x10
#658 @ 970,42: 16x27
#659 @ 475,229: 21x22
#660 @ 388,399: 14x20
#661 @ 264,76: 22x13
#662 @ 228,801: 28x28
#663 @ 728,673: 28x10
#664 @ 810,523: 24x28
#665 @ 168,171: 17x20
#666 @ 803,365: 5x5
#667 @ 618,818: 25x22
#668 @ 771,882: 19x26
#669 @ 123,608: 17x25
#670 @ 170,811: 21x15
#671 @ 460,484: 28x24
#672 @ 201,260: 23x11
#673 @ 575,420: 29x16
#674 @ 859,869: 11x27
#675 @ 318,293: 12x14
#676 @ 512,424: 25x18
#677 @ 911,844: 19x16
#678 @ 162,394: 25x29
#679 @ 194,307: 21x21
#680 @ 922,954: 22x16
#681 @ 704,797: 18x28
#682 @ 246,33: 29x10
#683 @ 421,256: 10x12
#684 @ 535,280: 27x17
#685 @ 147,353: 24x15
#686 @ 796,292: 25x10
#687 @ 550,653: 11x19
#688 @ 784,253: 29x22
#689 @ 368,446: 15x28
#690 @ 822,285: 18x25
#691 @ 263,58: 13x14
#692 @ 746,662: 23x26
#693 @ 589,693: 19x23
#694 @ 341,297: 16x12
#695 @ 60,665: 24x26
#696 @ 232,1: 12x24
#697 @ 603,710: 10x14
#698 @ 345,133: 29x28
#699 @ 939,930: 11x18
#700 @ 349,663: 13x16
#701 @ 457,140: 15x17
#702 @ 130,729: 15x26
#703 @ 756,241: 23x19
#704 @ 830,405: 20x19
#705 @ 759,954: 14x28
#706 @ 257,97: 24x11
#707 @ 826,886: 16x21
#708 @ 149,233: 21x25
#709 @ 776,231: 20x22
#710 @ 890,199: 22x28
#711 @ 344,218: 22x26
#712 @ 510,539: 22x27
#713 @ 785,543: 23x28
#714 @ 408,471: 20x10
#715 @ 189,695: 22x19
#716 @ 768,935: 18x23
#717 @ 676,349: 11x19
#718 @ 912,768: 21x11
#719 @ 395,76: 25x27
#720 @ 346,694: 22x12
#721 @ 170,1: 25x11
#722 @ 545,348: 28x13
#723 @ 164,819: 25x29
#724 @ 223,907: 14x24
#725 @ 601,422: 18x11
#726 @ 191,375: 29x19
#727 @ 844,936: 16x26
#728 @ 65,536: 10x29
#729 @ 91,17: 23x22
#730 @ 48,330: 17x22
#731 @ 379,817: 29x21
#732 @ 385,963: 25x26
#733 @ 665,685: 29x28
#734 @ 92,481: 19x21
#735 @ 550,658: 21x26
#736 @ 951,85: 17x20
#737 @ 387,738: 14x11
#738 @ 801,898: 26x17
#739 @ 949,589: 15x24
#740 @ 2,561: 23x20
#741 @ 314,672: 10x29
#742 @ 894,197: 28x26
#743 @ 244,627: 10x20
#744 @ 74,70: 13x25
#745 @ 516,746: 14x20
#746 @ 825,309: 13x14
#747 @ 146,323: 27x26
#748 @ 752,249: 29x11
#749 @ 529,131: 22x15
#750 @ 468,524: 14x23
#751 @ 0,580: 12x11
#752 @ 94,427: 14x22
#753 @ 41,245: 21x10
#754 @ 427,332: 27x21
#755 @ 796,519: 17x29
#756 @ 600,725: 23x12
#757 @ 577,393: 21x10
#758 @ 579,334: 17x14
#759 @ 874,953: 15x25
#760 @ 269,918: 6x7
#761 @ 765,286: 26x26
#762 @ 21,65: 12x17
#763 @ 560,99: 20x25
#764 @ 947,388: 21x21
#765 @ 237,449: 11x11
#766 @ 538,719: 17x16
#767 @ 425,949: 16x24
#768 @ 839,862: 11x15
#769 @ 89,947: 28x28
#770 @ 460,582: 25x16
#771 @ 618,882: 17x12
#772 @ 200,470: 23x13
#773 @ 36,593: 22x17
#774 @ 209,277: 10x10
#775 @ 615,57: 11x15
#776 @ 910,838: 10x18
#777 @ 864,275: 14x17
#778 @ 742,614: 12x21
#779 @ 805,721: 10x22
#780 @ 843,162: 21x17
#781 @ 167,280: 20x14
#782 @ 487,963: 27x14
#783 @ 572,379: 29x28
#784 @ 625,806: 6x5
#785 @ 405,117: 5x12
#786 @ 424,624: 19x26
#787 @ 332,519: 13x22
#788 @ 800,413: 13x24
#789 @ 597,381: 28x24
#790 @ 881,860: 16x24
#791 @ 866,308: 11x25
#792 @ 973,640: 21x18
#793 @ 480,493: 26x26
#794 @ 158,966: 12x10
#795 @ 526,439: 15x11
#796 @ 660,885: 13x10
#797 @ 97,478: 28x23
#798 @ 415,91: 17x15
#799 @ 495,746: 27x10
#800 @ 947,623: 13x11
#801 @ 229,417: 11x29
#802 @ 211,607: 29x15
#803 @ 789,233: 27x18
#804 @ 180,26: 14x26
#805 @ 462,487: 23x16
#806 @ 118,657: 29x10
#807 @ 98,713: 24x14
#808 @ 126,883: 23x15
#809 @ 724,894: 29x22
#810 @ 465,216: 27x28
#811 @ 792,199: 26x14
#812 @ 881,189: 12x19
#813 @ 885,721: 23x21
#814 @ 186,569: 26x12
#815 @ 74,473: 11x19
#816 @ 619,887: 26x11
#817 @ 691,369: 22x10
#818 @ 929,461: 14x28
#819 @ 864,664: 22x28
#820 @ 577,405: 29x21
#821 @ 678,361: 22x17
#822 @ 406,465: 27x11
#823 @ 601,555: 25x17
#824 @ 74,552: 25x13
#825 @ 31,780: 10x27
#826 @ 706,974: 28x11
#827 @ 873,806: 27x14
#828 @ 767,270: 26x26
#829 @ 617,801: 20x22
#830 @ 850,5: 11x23
#831 @ 770,28: 22x13
#832 @ 963,596: 17x20
#833 @ 47,697: 24x11
#834 @ 331,662: 23x18
#835 @ 859,25: 22x23
#836 @ 160,674: 10x14
#837 @ 788,612: 29x11
#838 @ 875,164: 26x25
#839 @ 832,75: 29x22
#840 @ 663,0: 10x21
#841 @ 723,342: 23x11
#842 @ 832,357: 11x23
#843 @ 624,436: 12x24
#844 @ 278,687: 18x23
#845 @ 681,382: 19x14
#846 @ 902,952: 25x20
#847 @ 787,838: 26x24
#848 @ 825,699: 25x17
#849 @ 919,285: 18x17
#850 @ 486,705: 19x24
#851 @ 222,444: 18x24
#852 @ 168,615: 14x15
#853 @ 179,944: 15x26
#854 @ 87,300: 21x22
#855 @ 815,974: 25x17
#856 @ 61,563: 10x15
#857 @ 538,374: 18x18
#858 @ 796,663: 23x14
#859 @ 354,346: 26x23
#860 @ 604,791: 23x19
#861 @ 460,730: 25x21
#862 @ 851,963: 28x12
#863 @ 91,501: 24x17
#864 @ 895,272: 3x5
#865 @ 853,362: 14x29
#866 @ 227,799: 20x11
#867 @ 315,306: 23x15
#868 @ 388,383: 29x21
#869 @ 84,810: 14x23
#870 @ 839,879: 23x22
#871 @ 262,876: 18x25
#872 @ 302,858: 23x14
#873 @ 309,542: 25x25
#874 @ 207,384: 20x29
#875 @ 534,699: 19x14
#876 @ 4,470: 21x10
#877 @ 508,823: 25x19
#878 @ 616,852: 20x15
#879 @ 20,14: 23x23
#880 @ 263,894: 20x21
#881 @ 554,486: 27x19
#882 @ 949,426: 15x22
#883 @ 373,737: 29x18
#884 @ 299,906: 25x26
#885 @ 827,173: 28x18
#886 @ 160,889: 8x14
#887 @ 425,181: 24x15
#888 @ 343,600: 14x29
#889 @ 462,875: 22x10
#890 @ 792,711: 28x28
#891 @ 823,844: 10x15
#892 @ 481,706: 27x29
#893 @ 971,681: 24x29
#894 @ 246,577: 18x18
#895 @ 971,976: 27x17
#896 @ 412,54: 18x25
#897 @ 563,797: 10x11
#898 @ 607,645: 18x11
#899 @ 481,537: 16x14
#900 @ 912,353: 14x21
#901 @ 625,860: 27x14
#902 @ 768,151: 12x17
#903 @ 124,132: 16x20
#904 @ 860,377: 27x10
#905 @ 659,553: 18x25
#906 @ 222,965: 26x27
#907 @ 822,194: 14x22
#908 @ 11,757: 13x29
#909 @ 272,780: 26x22
#910 @ 769,252: 29x15
#911 @ 549,33: 13x17
#912 @ 247,82: 28x12
#913 @ 711,354: 12x13
#914 @ 357,75: 21x16
#915 @ 148,554: 21x20
#916 @ 824,848: 15x17
#917 @ 531,611: 26x19
#918 @ 123,461: 29x17
#919 @ 42,18: 18x24
#920 @ 414,344: 26x29
#921 @ 572,419: 24x16
#922 @ 184,198: 17x14
#923 @ 484,290: 12x13
#924 @ 442,184: 14x17
#925 @ 400,462: 17x24
#926 @ 820,200: 29x21
#927 @ 745,873: 13x16
#928 @ 443,29: 29x28
#929 @ 876,595: 22x10
#930 @ 775,848: 29x10
#931 @ 231,410: 18x14
#932 @ 586,756: 24x22
#933 @ 532,548: 24x14
#934 @ 866,851: 12x11
#935 @ 566,775: 21x14
#936 @ 759,651: 10x22
#937 @ 921,717: 12x26
#938 @ 577,400: 24x13
#939 @ 172,379: 12x26
#940 @ 554,961: 25x23
#941 @ 219,890: 12x28
#942 @ 78,556: 7x4
#943 @ 241,239: 20x10
#944 @ 203,609: 13x14
#945 @ 613,661: 27x22
#946 @ 552,871: 26x14
#947 @ 587,954: 16x21
#948 @ 758,895: 15x22
#949 @ 309,910: 16x20
#950 @ 523,351: 19x25
#951 @ 910,541: 22x29
#952 @ 929,965: 26x15
#953 @ 394,259: 24x13
#954 @ 857,955: 23x23
#955 @ 761,470: 15x24
#956 @ 551,329: 23x13
#957 @ 520,707: 28x20
#958 @ 402,345: 16x24
#959 @ 507,837: 11x15
#960 @ 462,816: 16x29
#961 @ 88,388: 19x25
#962 @ 816,588: 24x26
#963 @ 262,862: 27x29
#964 @ 621,802: 15x17
#965 @ 335,546: 20x15
#966 @ 847,17: 28x28
#967 @ 312,275: 26x20
#968 @ 562,133: 11x18
#969 @ 60,106: 25x21
#970 @ 554,676: 16x11
#971 @ 776,808: 22x27
#972 @ 771,602: 12x14
#973 @ 519,689: 23x19
#974 @ 740,212: 13x11
#975 @ 261,212: 13x14
#976 @ 770,135: 18x24
#977 @ 731,593: 4x4
#978 @ 382,229: 15x29
#979 @ 395,839: 21x14
#980 @ 549,717: 20x14
#981 @ 376,702: 29x10
#982 @ 331,822: 12x18
#983 @ 683,153: 24x29
#984 @ 552,668: 29x13
#985 @ 943,92: 10x11
#986 @ 895,601: 21x11
#987 @ 332,636: 15x24
#988 @ 655,432: 23x20
#989 @ 61,885: 29x13
#990 @ 160,674: 10x29
#991 @ 435,359: 17x15
#992 @ 882,576: 20x24
#993 @ 864,288: 12x11
#994 @ 287,294: 21x16
#995 @ 831,380: 24x11
#996 @ 809,316: 22x29
#997 @ 183,179: 17x12
#998 @ 922,108: 11x27
#999 @ 782,426: 16x24
#1000 @ 333,200: 19x24
#1001 @ 717,338: 13x22
#1002 @ 421,89: 17x17
#1003 @ 880,672: 19x10
#1004 @ 891,37: 29x28
#1005 @ 830,352: 26x11
#1006 @ 436,65: 25x26
#1007 @ 280,772: 27x26
#1008 @ 579,395: 18x25
#1009 @ 670,158: 19x29
#1010 @ 222,564: 26x29
#1011 @ 447,503: 28x22
#1012 @ 622,853: 12x10
#1013 @ 473,777: 12x25
#1014 @ 674,335: 22x16
#1015 @ 888,93: 18x20
#1016 @ 205,309: 12x20
#1017 @ 809,902: 23x19
#1018 @ 50,776: 11x28
#1019 @ 297,406: 11x27
#1020 @ 750,578: 16x24
#1021 @ 887,869: 22x28
#1022 @ 472,673: 28x28
#1023 @ 966,446: 12x25
#1024 @ 792,890: 11x17
#1025 @ 912,357: 14x24
#1026 @ 576,966: 16x15
#1027 @ 949,252: 19x12
#1028 @ 969,589: 26x25
#1029 @ 182,39: 18x25
#1030 @ 967,699: 26x19
#1031 @ 579,821: 29x19
#1032 @ 763,937: 17x10
#1033 @ 35,110: 10x26
#1034 @ 842,80: 10x13
#1035 @ 932,609: 29x11
#1036 @ 800,665: 20x16
#1037 @ 332,635: 28x18
#1038 @ 635,818: 27x18
#1039 @ 271,822: 12x23
#1040 @ 906,342: 26x25
#1041 @ 831,224: 22x10
#1042 @ 619,718: 15x27
#1043 @ 725,325: 26x13
#1044 @ 684,336: 23x16
#1045 @ 804,877: 19x19
#1046 @ 80,263: 28x13
#1047 @ 501,280: 21x29
#1048 @ 624,243: 12x12
#1049 @ 453,322: 28x23
#1050 @ 257,523: 29x24
#1051 @ 211,376: 26x19
#1052 @ 590,821: 14x17
#1053 @ 454,551: 23x12
#1054 @ 580,174: 20x15
#1055 @ 872,367: 11x14
#1056 @ 44,557: 25x20
#1057 @ 303,586: 12x29
#1058 @ 493,547: 11x11
#1059 @ 466,586: 10x24
#1060 @ 219,872: 14x3
#1061 @ 932,858: 26x27
#1062 @ 63,51: 27x25
#1063 @ 942,293: 25x28
#1064 @ 101,585: 29x11
#1065 @ 873,280: 18x28
#1066 @ 717,208: 26x17
#1067 @ 915,446: 10x3
#1068 @ 941,969: 11x10
#1069 @ 217,793: 26x13
#1070 @ 176,29: 22x10
#1071 @ 542,436: 25x18
#1072 @ 619,820: 16x25
#1073 @ 143,362: 14x14
#1074 @ 541,123: 11x13
#1075 @ 413,752: 23x27
#1076 @ 293,310: 13x18
#1077 @ 860,324: 28x13
#1078 @ 423,737: 15x16
#1079 @ 176,807: 25x22
#1080 @ 279,309: 18x22
#1081 @ 101,316: 10x28
#1082 @ 399,743: 12x29
#1083 @ 825,961: 27x25
#1084 @ 533,156: 28x12
#1085 @ 909,344: 12x20
#1086 @ 612,720: 21x27
#1087 @ 384,316: 15x14
#1088 @ 872,566: 18x23
#1089 @ 473,407: 23x25
#1090 @ 464,747: 13x10
#1091 @ 353,794: 28x13
#1092 @ 338,293: 25x21
#1093 @ 148,465: 23x11
#1094 @ 137,548: 14x16
#1095 @ 276,486: 17x28
#1096 @ 413,103: 14x10
#1097 @ 822,502: 19x26
#1098 @ 766,863: 23x26
#1099 @ 911,601: 22x13
#1100 @ 685,835: 29x13
#1101 @ 974,407: 18x26
#1102 @ 912,735: 29x18
#1103 @ 262,136: 24x13
#1104 @ 319,422: 20x15
#1105 @ 147,345: 27x12
#1106 @ 213,411: 10x16
#1107 @ 698,387: 20x18
#1108 @ 832,356: 24x10
#1109 @ 416,375: 21x25
#1110 @ 714,746: 22x25
#1111 @ 787,262: 28x16
#1112 @ 820,86: 18x29
#1113 @ 217,401: 10x14
#1114 @ 254,154: 12x19
#1115 @ 343,713: 15x11
#1116 @ 493,154: 26x17
#1117 @ 255,145: 28x13
#1118 @ 592,707: 13x13
#1119 @ 96,323: 29x18
#1120 @ 815,488: 25x26
#1121 @ 552,132: 26x20
#1122 @ 482,784: 24x11
#1123 @ 805,473: 27x26
#1124 @ 787,214: 25x22
#1125 @ 963,486: 28x10
#1126 @ 84,573: 22x17
#1127 @ 829,694: 17x23
#1128 @ 636,241: 27x27
#1129 @ 879,703: 17x16
#1130 @ 937,444: 17x10
#1131 @ 584,698: 28x17
#1132 @ 363,929: 28x21
#1133 @ 160,264: 29x10
#1134 @ 55,574: 10x20
#1135 @ 848,874: 22x27
#1136 @ 758,500: 19x20
#1137 @ 703,881: 29x22
#1138 @ 165,179: 26x20
#1139 @ 550,316: 10x21
#1140 @ 694,730: 18x12
#1141 @ 781,188: 14x21
#1142 @ 965,631: 27x21
#1143 @ 387,345: 17x5
#1144 @ 898,369: 11x29
#1145 @ 971,140: 12x26
#1146 @ 678,136: 14x26
#1147 @ 342,703: 4x4
#1148 @ 869,283: 22x24
#1149 @ 297,840: 22x15
#1150 @ 75,738: 14x23
#1151 @ 565,144: 22x17
#1152 @ 487,392: 27x19
#1153 @ 673,799: 27x26
#1154 @ 140,481: 14x12
#1155 @ 987,28: 11x17
#1156 @ 693,384: 14x25
#1157 @ 879,260: 19x29
#1158 @ 766,482: 15x21
#1159 @ 945,390: 19x21
#1160 @ 245,566: 15x20
#1161 @ 943,588: 20x27
#1162 @ 415,961: 25x15
#1163 @ 180,887: 12x10
#1164 @ 552,276: 21x25
#1165 @ 888,366: 11x21
#1166 @ 712,691: 12x26
#1167 @ 133,561: 22x18
#1168 @ 889,79: 24x15
#1169 @ 548,324: 16x24
#1170 @ 604,802: 20x26
#1171 @ 483,705: 19x10
#1172 @ 370,487: 23x14
#1173 @ 418,314: 22x11
#1174 @ 184,194: 10x25
#1175 @ 318,694: 10x14
#1176 @ 883,477: 12x29
#1177 @ 694,753: 18x12
#1178 @ 868,573: 13x21
#1179 @ 322,704: 28x29
#1180 @ 828,946: 19x11
#1181 @ 471,588: 20x15
#1182 @ 605,809: 27x23
#1183 @ 887,47: 14x24
#1184 @ 135,625: 26x10
#1185 @ 965,823: 12x17
#1186 @ 528,268: 17x20
#1187 @ 891,266: 27x15
#1188 @ 751,944: 22x20
#1189 @ 162,965: 24x27
#1190 @ 427,500: 23x15
#1191 @ 827,117: 19x19
#1192 @ 968,468: 29x23
#1193 @ 469,953: 25x15
#1194 @ 151,532: 18x18
#1195 @ 166,908: 25x18
#1196 @ 604,626: 19x10
#1197 @ 261,588: 16x17
#1198 @ 103,475: 27x29
#1199 @ 166,38: 19x24
#1200 @ 505,253: 24x14
#1201 @ 802,569: 15x22
#1202 @ 332,701: 18x27
#1203 @ 387,471: 29x21
#1204 @ 494,65: 25x20
#1205 @ 241,528: 11x17
#1206 @ 485,316: 27x12
#1207 @ 911,443: 25x10
#1208 @ 394,729: 27x28
#1209 @ 911,614: 24x26
#1210 @ 62,753: 16x19
#1211 @ 36,166: 11x16
#1212 @ 804,769: 23x21
#1213 @ 785,109: 27x26
#1214 @ 423,796: 18x14
#1215 @ 544,105: 21x14
#1216 @ 450,822: 13x26
#1217 @ 221,258: 13x11
#1218 @ 986,480: 12x14
#1219 @ 792,772: 29x21
#1220 @ 775,0: 14x14
#1221 @ 470,526: 3x18
#1222 @ 127,600: 25x21
#1223 @ 370,309: 19x12
#1224 @ 283,677: 19x27
#1225 @ 186,99: 18x15
#1226 @ 2,502: 27x11
#1227 @ 418,244: 11x27
#1228 @ 282,431: 18x18
#1229 @ 797,829: 22x23
#1230 @ 874,961: 11x16
#1231 @ 436,951: 16x28
#1232 @ 688,702: 19x22
#1233 @ 162,406: 23x18
#1234 @ 454,875: 19x16
#1235 @ 143,620: 24x21
#1236 @ 19,612: 3x8
#1237 @ 285,724: 17x14
#1238 @ 394,789: 11x29
#1239 @ 152,471: 26x24
#1240 @ 145,345: 23x20
#1241 @ 940,740: 20x16
#1242 @ 126,644: 28x27
#1243 @ 870,389: 17x28
#1244 @ 209,929: 11x21
#1245 @ 145,549: 25x16
#1246 @ 656,125: 28x28
#1247 @ 73,233: 17x18
#1248 @ 695,306: 23x21
#1249 @ 606,784: 20x19
#1250 @ 323,402: 19x24
#1251 @ 333,268: 26x14
#1252 @ 674,573: 24x24
#1253 @ 680,461: 21x12
#1254 @ 111,104: 20x27
#1255 @ 572,168: 11x19
#1256 @ 820,295: 13x23
#1257 @ 630,649: 12x15
#1258 @ 617,39: 12x26
#1259 @ 112,115: 22x22
#1260 @ 349,123: 28x29
#1261 @ 709,905: 16x25
#1262 @ 264,173: 10x19
#1263 @ 265,161: 16x18
#1264 @ 341,820: 11x13
#1265 @ 791,516: 28x25
#1266 @ 180,964: 28x20
#1267 @ 885,99: 20x20
#1268 @ 950,683: 17x15
#1269 @ 623,444: 28x13
#1270 @ 140,347: 17x19
#1271 @ 819,286: 11x25
#1272 @ 873,39: 24x26
#1273 @ 627,246: 4x4
#1274 @ 822,30: 18x22
#1275 @ 944,541: 14x10
#1276 @ 428,750: 10x12
#1277 @ 772,197: 13x26
#1278 @ 946,92: 12x16
#1279 @ 716,814: 20x13
#1280 @ 613,132: 23x14
#1281 @ 157,674: 25x17
#1282 @ 516,242: 27x24
#1283 @ 390,559: 19x17
#1284 @ 456,724: 11x14
#1285 @ 165,357: 26x23
#1286 @ 526,47: 29x26
#1287 @ 518,912: 19x10
#1288 @ 966,207: 28x25
#1289 @ 846,220: 15x17
#1290 @ 99,667: 6x8
#1291 @ 195,51: 27x24
#1292 @ 872,954: 22x25
#1293 @ 436,23: 23x22
#1294 @ 79,315: 10x20
#1295 @ 505,500: 17x16
#1296 @ 434,715: 14x25
#1297 @ 175,402: 8x8
#1298 @ 563,354: 12x10
#1299 @ 184,458: 25x15
#1300 @ 454,347: 18x27
#1301 @ 888,82: 19x22
#1302 @ 967,661: 21x18
#1303 @ 188,899: 19x11
#1304 @ 136,343: 13x14
#1305 @ 806,336: 14x20
#1306 @ 362,578: 16x21
#1307 @ 342,827: 23x13
#1308 @ 111,552: 26x15
#1309 @ 464,781: 20x21
#1310 @ 700,823: 22x25
#1311 @ 768,26: 25x13
#1312 @ 410,853: 19x23
#1313 @ 273,313: 24x21
#1314 @ 91,183: 14x17
#1315 @ 646,354: 17x21
#1316 @ 494,57: 20x22
#1317 @ 382,340: 28x29
"
           ))

(def day2 (str/split-lines
            "xdmgyjkpruszabaqwficevtjeo
xdmgybkgwuszlbaqwfichvtneo
xdmgyjkpruszlbcwwfichvtndo
xdmgcjkprusyibaqwfichvtneo
xdmgyjktruszlbwqwficuvtneo
xdmgxjkpruszlbaqyfichvtnvo
xdmgytkpruszlbaqwficuvtnlo
xdmgydkpruszlbaqwfijhvtnjo
xfmgyjkmruszlbaqwfichvtnes
xdmgyrktruszlraqwfichvtneo
xdmgyjkihuszlbaqdfichvtneo
hdmgyjkpruszeiaqwfichvtneo
xdmzyjkpruszlbaqwgichvtnxo
xdmgyjknquszlbpqwfichvtneo
idmgyjrpruszlbtqwfichvtneo
xkmgyjkpruuzlbaqwfichvfneo
xdmgyjkpruszlfaqwficnvtner
xdmgyjkpruszlbpqwficwvteeo
xdmgyjkpwuszlbiqwfhchvtneo
xdmgyjkpruszwbaqwfichrtnbo
xdpgyjkprusblbaqwfgchvtneo
xdmryjkcruszlbaqwfichvtnee
xwmgylkpruszlbaqwfcchvtneo
xdmgyjkpruszflaqwfixhvtneo
xdmgyjkmruszloaqwfichvteeo
xvmgrjkpruszlbaqwfichvsneo
xdmvyjkprusmlbaqwfichvtnes
xdmgyjkpruszlbaqwfichkgbeo
xdmgyikpruxzlbaqwfichvtnei
xdmgyjkprugzlbaqhfichvtveo
xdmgyjkpruszlbaqjaichftneo
xdmzijkpruszlbaqwwichvtneo
xdmgyjkprsszlbaqwfihhvlneo
xdmgyjkprusqlwaqzfichvtneo
ximgyjkpruszlbawwfichvtnen
xsmgyjzpruszlbaqwfichvaneo
xdmgyjkpruszlcaoyfichvtneo
xdmgyjkprusmlbaqvnichvtneo
xdmgyjkvruszmbaqwfichvtueo
xdmgyjppuuszleaqwfichvtneo
xddgyjkprubzlbaqwfichvaneo
xdmgwjkpruszebaswfichvtneo
xdogyjkpruszlblqwfichvdneo
xdkgyjgpruszlbaqwfizhvtneo
xdvgyjkpruszlbdqwfichvtqeo
xdmgyjlpruszlbapwficgvtneo
xdmgyjkpruszlbaqofickvtngo
xdmgyjkprqszliaywfichvtneo
xdqgyjkpruszlbcqwficnvtneo
xdmgdjkpruszlbaqwxichvtseo
xdmgyjkpruczlbaqwfichdtnfo
xdmgyjkpruszluaqwficzvtnjo
xdmgyjkproszlbaqwfacevtneo
xfmgijkpruszlbrqwfichvtneo
odmgyjkpluszlbaqwfichvuneo
xdmgyjkpruszlbaqwwichukneo
xdmgdjkpruszwbaqwfichvtnet
xdmgyjkzrusvlbaqwrichvtneo
xdmgylkprutzlbaqwfichvtnbo
xdmgyjkpruszsbaqwfijtvtneo
xdmgyjkproszlbjqwfichntneo
xdmgyhkpluszlbaqwfichvtnlo
xdmgyjhprushlbaqwfichvtnzo
gdmoyjkpruszlbarwfichvtneo
cdmgyjkpruszlbaqwfcchvtned
xgmgyjkpruszlbaqwfschvtnek
xdmgyjkprusnlzamwfichvtneo
xdmgyjkprgszlbaxwfichvuneo
txmgyjksruszlbaqwfichvtneo
xdmgyjkprusbbbpqwfichvtneo
xdmoyjkpruszlbaqwfighvtxeo
xdmgyjkpruslhbaqwfichptneo
xdmgzjkpruszlbaqwffcmvtneo
xdmgyjkiruszlbaqgficuvtneo
vdbgyjkpruszlbaqwfichvtnek
xdmgyjspruszlbaqwfochvtney
xdmgyjkpruszibaqwfivhvteeo
xdmgyjkpruszfbaqwficbvtgeo
xdmgyjkprystlbaqwxichvtneo
xdmfyjkpryszlxaqwfichvtneo
xdmgyjgpruspybaqwfichvtneo
xdmgyjklruszlbjqwdichvtneo
xdmgyjkzruszltaqwfichvtnek
xdmgqjkpruszlzaqwfichvtneh
xdmgyjhnruszlbaqwficqvtneo
xdmgyjkproszlbaqweichvtnez
xdmgyjkprurzlbaawfichytneo
xdmgyfkpruszlbaqwfschutneo
xdmnyjkpruszlbaawjichvtneo
xdmgyjkpybszlbaqwfichvwneo
xdmgtjkhruszlbaqwfichatneo
xamgyjkprurzlbaqwfichvaneo
xdmgyjkpruszlbaqwgichvtnqv
ndmgyjkpruszlsaqwfuchvtneo
xdmgygkpgusrlbaqwfichvtneo
xdmgyjkpruszfbaqwfichvtnmy
xdmgyjkprupflbaqwfichvjneo
ndmgyjkpruszlbagwfichvtnxo
xdmgyjkpruszlbafwfilhvcneo
xdmgyjkpruszlbaqwfichvjsea
xebgyjkpruszlbaqafichvtneo
xdmkyjdpruszlbaqwfichvtnei
xomgyjkprufzlbaqwfochvtneo
xdmgyjkprfsllbaqwfiihvtneo
xdmyyjkpruszebaqwficmvtneo
xdmnyjkpruczlbarwfichvtneo
xdmgyjkpruszcbaqwbichvtneg
xdmgxjkpluszlbapwfichvtneo
xgrlyjkpruszlbaqwfichvtneo
xdmgyjkpruszlraqwxcchvtneo
xdmhyjupruszlbaqafichvtneo
xdmgnjkpruszlbkqwfjchvtneo
xdmgyjkpruszlwaqwfichvtndg
xdmgfjkpruvqlbaqwfichvtneo
xdmgejkptuszlbdqwfichvtneo
xlmgyjkpruszlnaqwfochvtneo
xdmgcjkpruszlbaqwfiqhvaneo
xdmgyjupruyzlbaywfichvtneo
gdmgyjkpruyzlbaqwficevtneo
xdmgyjkaruazlbapwfichvtneo
xsmiyjkpruszlbaqwfichvtveo
xdmiyjkprukzlbaqwfichvtnea
xdbgmjkxruszlbaqwfichvtneo
xdmgyjkpruskvbaqwfichdtneo
xdmgyjkprusznbaqwficshtneo
xdmgyjkprusrlbaqwfzchetneo
xdmgyrkpruszzbaqwfichvtned
xdmgyjkprusolbacwmichvtneo
xdmgypkpruszlbaqwfichvtmgo
xdmgyjkprumzlbhqwfichttneo
xdmgydkprusglbaqwfichvtnei
xdmuyjkpruszlbpqwfichvtyeo
xdmtymkprusslbaqwfichvtneo
xdmgyjjprkszlbaqwfqchvtneo
xdmgvjdpruszlbaqwfichgtneo
xdtgyjkpruwzlbaqwfjchvtneo
xdmgyjkpruszlbafseichvtneo
xdmgvjkpruszlraawfichvtneo
xdmgyukprgszlbatwfichvtneo
xhmgyjkpruszliaqwnichvtneo
xdmgyjspruszlbwqyfichvtneo
xdmgyjkjruszlqaqwfichvtnvo
xdmgyjkiruszlbnqwfichmtneo
ximgyjkpruszlbaqwfvcevtneo
xdmdyjkpruszlbaqwsithvtneo
ndmgyjkpruszlbaqwfilhatneo
xdmgyjkpruszlbaqwfinhvcnez
xdmgypkpsuszlbajwfichvtneo
xdpgmjkpluszlbaqwfichvtneo
xdmgyjnprupzlbaqwfichvtnel
xbmgyjkprmszlfaqwfichvtneo
xdmgyjkpausllbaqwfichvtseo
xdmgyjkpruszlbaqwfqchttnes
xgmgyjkpruszlbaxwfichvtneb
xdmgyjkpruszabqqwfichvineo
xdmgpjkpquszlbaqwfichvdneo
xdmgyjkeruszlbaqdficbvtneo
xdmaujkpruszlbaqwfichvteeo
xdmgyjkpruszlbaqwrirhvtnev
xdmgyjkpsugzllaqwfichvtneo
xdmgyjkpruszlbaqwfichctnlm
xdmeyjkpruszlbacwfiwhvtneo
xdmgyjkpiuhzlbaqwfijhvtneo
xdmgyjkpruszlbmqhfiohvtneo
xdegyjkpbuszlbbqwfichvtneo
xdmggxkpruszlbaqwfirhvtneo
xdmgojkpruszlbaqvfichvtteo
xdmgyjhtruszlbaqwmichvtneo
rdmgyjkpruszlbaqwfichvthek
xdlgyjqpruszlbaqwfbchvtneo
xdmgyjspriszlbavwfichvtneo
rdkgyjkpruszlbaqwfichvtnuo
tdmgyjkuruszlbaqwfichvtnev
xdmgyjkpxuszlbaqwfkchvtnso
xdegyjkpruszlbbqxfichvtneo
xdmgyjkpruszlbaqwficpvtket
xdmgyjkpruszliaqwfnchvtnec
xdmgyjkpreszlbaqwficdvtdeo
rdmgyjkpruszlbaywfychvtneo
xdmgywkpruszlbaqwficrvtaeo
xdmgyjkpruszlbanwflchvoneo
xdmgyjkpruyzlbaqufychvtneo
symgyjkpruszlbaqwfichvtqeo
xdmgyjkpruszlbaqwfichvbzqo
xzfgyjkpruszlbaqwfichvtveo
udmgyjepruszlbaqwfichbtneo
xhmgyjkpruszlbaqwfjchvtnef
xdhgyjkpruszlbaqaftchvtneo
xdmzyjkjruszlbaqwfichvtnwo
xdmgyjepruszlbaqwffchvtnef
xdmgyjkprurzlbaqwfikhvtneq
xomoyjkpruszkbaqwfichvtneo
xdmgyjkpiuszubaqwfichktneo
xdmgyjkprusdlbaqwhihhvtneo
xdmgyjkpruszlbaqwwirhvxneo
xdmgyjkpruszlbaqwficgitzeo
xdmgyjlpruszlbaqwfichpjneo
xjmgyjkpxuszlbaqwfichatneo
xdmgylkpruszlbaqwfiehvtnez
xdmgbjkpruszmbaqwfihhvtneo
xdmgyjkprubzlwaqwfichvtxeo
xdmgyjhlrustlbaqwfichvtneo
xdmmyjkpruszlbaqwfdchitneo
xdmgyjkpruszlbaqwoichhtbeo
xdzgyjkprvszlcaqwfichvtneo
ndmgyjkpruszlbaqwficavxneo
xdmgyjfpruszxbaqzfichvtneo
xdmgyjkpeuszlbaqzficdvtneo
xdmgyjkpruszlbmqffidhvtneo
xdnvyjkpruszlbafwfichvtneo
xdygyjkpruszlbljwfichvtneo
xdigyjkpruszlbeqwfuchvtneo
xdmgyjkpruszlbpzwfichvteeo
bdmgyjbpruszldaqwfichvtneo
xdmgyjkprrszlbaqmpichvtneo
idmgyjkpruszlbaqyfichvtkeo
xdmgyjkmrqsclbaqwfichvtneo
xdmgyjkpruazlbeqwfichvtxeo
ddmgyjkpruszlbsqwfichotneo
xdmgyqkpruszjbaqwfxchvtneo
xdmnyjkpruozlbaqwfichvtreo
edmgyjkpruszlbuqwwichvtneo
xdmgyjkprmshlbaqwfichctneo
xdmgyjkpruszlbaqwffghotneo
xdmcyjkprfszlbaqnfichvtneo
xdmgyjypruszhbaqwficyvtneo
xdmgyjkprzszlyaqwficmvtneo
xlmgyjkprzszlbaqwficyvtneo
xdmgyjkprutulbaqwfithvtneo
xdygyjkpruszlbpqwfichvpneo
xdmgsjkpoumzlbaqwfichvtneo
xdmgyjkpyuszlbaqdfnchvtneo
xdxgyjkpruszlbaqwfizhvtnjo
xdmgyjkpruszlbaqwfschvkndo
xdmgpjkprnszlcaqwfichvtneo
xhmgyjkpruszlbaqwficgvtnet
xdmgyjkpruswlbaqwfichvtqer
ddmgyjkprcszlbaqwfichqtneo
xdmgyjkpruhhlbaqwpichvtneo
xdmgyjkeraszlbaqwfichvtnso
nomgyjkpruszlbaqwficavxneo
xdmgyjkprdszlbaqwfobhvtneo
xdmgyjkprgszlbaqwfichvtdao
xomgyjspruswlbaqwfichvtneo
xdzgyjkpruszlbaqwficwvpneo
admgejkpruszlbaqwfimhvtneo
xdtgyjkpruszlmaqwfiqhvtneo
xdmgymkprusqlbaqwtichvtneo
xdmgyjkpluszlbaqwfidhvtnea
ztmgyjjpruszlbaqwfichvtneo
"))

(def day1
  "-4
+7
+3
+1
-9
-14
+18
-7
-5
-18
+11
-8
+17
-16
-19
+14
+11
-8
+14
+22
+13
+14
-18
+8
-16
+10
-12
+9
-19
-12
-6
+10
+2
-14
+18
+17
+11
-5
+6
+9
+16
-3
+12
+5
+15
+7
+2
-5
-13
+7
+19
+10
-2
+3
-5
-7
-11
+14
+13
+3
+11
+15
-19
-1
-5
+15
+14
-16
+8
+9
-11
-7
+15
+4
+7
+11
-2
+17
-8
-14
+3
-4
+18
+1
+6
-5
+17
+13
-14
-15
-9
+16
+14
+12
-14
-14
+4
-19
+11
+5
+7
+1
+13
-7
+19
+12
+10
+13
-3
+6
-17
+13
-8
+16
+4
-15
+14
-1
+15
-19
+15
-19
+17
-6
+11
-10
+9
+17
+6
+15
-18
+2
-8
+11
-6
-7
+9
+16
-13
-18
-1
+5
-16
+9
-6
+11
-16
+3
-9
+18
+5
+7
+15
+12
+19
-17
-4
-15
-13
+5
-11
+25
+14
+5
+5
+4
-2
-4
-14
+7
+16
+6
+11
-8
+13
+8
+3
-12
+8
+7
-5
+14
+5
+8
-3
-14
-14
-7
-18
-12
+18
+4
-14
-17
-17
+12
-9
+3
-18
+27
+26
-9
-7
+12
+9
+6
+7
+1
-13
+4
-8
-3
-12
+21
+13
+6
+7
+9
+18
-17
+4
+10
-11
-17
-16
+18
+9
-2
+3
-13
-9
+3
-10
-15
+11
+24
+2
+16
+11
+18
+14
-5
-3
+18
+4
+4
+5
-2
-10
+8
+19
+4
+14
+8
+1
-14
-7
+15
-4
-5
+20
+15
+2
-14
-12
-16
+4
-9
+18
+16
+7
+7
+10
+17
-18
+3
+10
-11
-21
-9
-2
+12
-17
+13
+19
-18
-4
-19
+10
-11
-18
-4
+6
+1
+2
-4
-28
-28
-19
+13
+18
-15
-6
+5
+9
-26
-6
+19
-6
+1
-11
-11
+25
-26
+19
+10
+2
+9
-22
-66
-8
+1
-14
-2
-5
+19
-55
+6
-19
-4
-13
+21
-13
-3
-10
-7
+15
-5
-4
-10
+5
-11
-3
+16
+6
+10
+3
-1
-5
-21
-6
+9
-4
-6
-17
-10
+14
-11
+17
+16
-19
+6
+1
-2
+8
-15
+17
-14
-16
+7
-5
-24
-7
-20
+5
-4
+11
-1
+12
-9
-11
-9
-9
-11
-3
-15
-21
-12
+21
+3
+12
+2
+2
+21
-9
+20
+4
+11
-1
+4
-20
+4
+19
+12
+4
-15
+14
+11
+21
-29
-53
+10
-11
+8
-10
-11
+12
-17
-25
-35
+1
-2
+14
+36
+70
+14
+50
+3
+6
+23
-20
-6
-66
-39
-14
+191
-17
+24
+155
-6
+61
+59623
-4
+7
+19
+5
-1
-8
-8
-15
+16
+8
-7
+19
-3
+14
+14
+6
+17
-15
+3
+11
+12
-5
+19
+2
+1
+16
+13
+6
+1
+18
+16
-4
+11
-1
+19
-7
-19
-10
+19
+7
+4
-17
-4
-4
+9
-8
-17
-9
-14
+16
+4
-19
+11
+1
+2
-19
+7
-8
-7
-18
-6
+4
-5
-15
+2
+7
-18
-8
+14
-20
+1
+17
-2
+3
+17
-1
+7
+10
+10
-1
+19
+3
-16
-10
+1
+10
+6
+20
-17
+19
+14
-18
-11
+22
-3
-16
+15
+12
+14
-12
-1
+22
+10
-7
-8
+2
-1
-6
-7
+19
+7
+12
-14
+18
+7
-17
-9
+10
+14
-1
-17
+8
+4
+2
+16
+3
+16
+4
-2
+18
-19
-15
-1
-10
-5
+8
-4
-13
+18
+15
-5
+10
-6
+13
-11
+12
+2
+4
-2
-13
-7
-11
-13
-5
+4
+10
+12
-15
-16
-10
-9
-27
-4
-9
-21
-13
-4
-1
-10
-10
+14
+12
-29
-16
-12
-1
-12
+18
-20
-11
-5
+12
-2
-6
+18
-2
-1
+6
-18
-12
+20
-4
+6
-14
+6
-17
-6
+8
-14
-18
-4
-8
+2
+7
+20
-10
-15
+18
-12
+13
-18
+9
+11
-19
+9
+3
+10
-17
-16
-4
-11
-5
-9
-12
+4
-13
+14
+13
-17
-21
+7
+19
+10
-6
+9
+13
-5
-13
+19
+7
-4
-6
+17
-16
-8
-14
-11
-3
-16
+10
-1
-2
-19
+3
+13
-15
+8
+6
-19
+2
-12
-6
-19
+17
-8
+11
+2
-18
-6
-10
+7
+4
+12
-3
+8
+7
+18
-8
+10
-15
-2
-12
+20
-13
-16
-11
+9
-1
+11
+19
-5
+20
-2
+17
+17
+17
+18
+17
+25
-31
+20
-7
+21
+15
+15
-13
+18
+14
+28
-10
+16
-17
-16
-2
-7
-8
-23
-16
+11
-20
+11
-3
-31
-4
-20
+18
-8
+3
+6
-15
-35
+19
+2
-4
+9
-12
-15
-9
-3
+15
+38
-6
+11
-36
-11
-14
-1
-5
+2
+23
-12
-33
-18
+1
+5
+3
+16
+7
-28
+8
-22
-7
-18
-17
+16
+11
+16
+4
-15
+7
-10
-7
-9
-4
+9
+6
-21
-11
-20
-19
-5
+3
+27
+9
+23
+16
-19
-2
+20
+13
-2
-16
+11
+2
-10
-11
-21
-6
-8
-10
-20
+35
+46
+2
-19
-13
+23
-2
+56
+7
+53
+5
-64
-62
+103
+162
+4
+10
-27
-15
-14
+22
+9
+1
+38
-9
+8
+9
+34
+1
-11
+4
-19
-29
+16
+52
-16
+27
+188
-83
+191
+59071
+19
-10
-6
-16
-18
+14
-10
+18
+4
-10
+3
+16
+6
-18
-3
+1
+4
-20
+4
-16
-7
-6
+19
+12
-3
-7
-13
-7
-9
+19
-20
+12
+12
+18
-15
-8
-11
-9
-7
+13
+16
+11
+4
-13
-10
+1
-11
-14
+13
+4
+10
-5
+7
-24
-7
+11
-22
-20
+15
-10
+13
-7
-25
-2
-4
+45
-1
+16
+11
+24
+4
+4
+22
-17
+13
-16
+5
+16
+16
+15
-119289")
