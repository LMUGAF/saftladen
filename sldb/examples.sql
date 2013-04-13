-- phpMyAdmin SQL Dump
-- version 3.3.7deb7
-- http://www.phpmyadmin.net
--
-- Host: 
-- Generation Time: Mar 09, 2013 at 06:46 PM
-- Server version: 5.1.66
-- PHP Version: 5.3.3-7+squeeze14

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: ``
--

-- --------------------------------------------------------

--
-- Table structure for table `product`
--

CREATE TABLE IF NOT EXISTS `product` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `ean` varchar(255) NOT NULL,
  `price` int(11) NOT NULL COMMENT 'in cent',
  `amount` int(10) unsigned NOT NULL DEFAULT '1' COMMENT 'derzeit nur 1 (enabled) oder 0 (disabled)',
  `volume` int(11) DEFAULT NULL COMMENT 'in cl',
  `caffeine` int(11) NOT NULL DEFAULT '0' COMMENT 'in mg/l',
  `alcohol` decimal(2,1) NOT NULL DEFAULT '0.0' COMMENT 'in volumenprozent',
  `note` text NOT NULL,
  `description` text NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=17 ;

--
-- Dumping data for table `product`
--

INSERT INTO `product` (`id`, `name`, `ean`, `price`, `amount`, `volume`, `caffeine`, `alcohol`, `note`, `description`) VALUES
(1, 'Paulaner Spezi 0,5l', '4066600603405', 66, 1, 500, 70, '0.0', 'caffeine geschätzt', ''),
(2, 'Original Spezi 0,5l', '41045848', 66, 1, 500, 66, '0.0', '', ''),
(3, 'Jever 0,5l', '4008948027000', 100, 1, 500, 0, '4.9', '', ''),
(4, 'Flora Power 0,5l', '4260031874056', 133, 1, 500, 180, '0.0', '', ''),
(5, 'Club Mate 0,5l', '4029764001807', 100, 1, 500, 200, '0.0', '', ''),
(6, 'Augustiner Hell 0,5l', '4105250022003', 100, 1, 500, 0, '5.2', '', ''),
(7, 'Tegernseer Spezial 0,5l', '4022396000033', 100, 1, 500, 0, '5.6', '', ''),
(8, 'Franziskaner Weißbier 0,5l', '4072700001126', 100, 1, 500, 0, '5.0', '', ''),
(9, 'Paulaner Weißbier 0,5l', '4066600641964', 100, 1, 500, 0, '5.5', '', ''),
(10, 'Afri Cola 1,0l', '4009228250477', 100, 1, 1000, 250, '0.0', '', ''),
(11, 'Red Bull 0,25l', '90162565', 133, 1, 250, 320, '0.0', '', ''),
(12, 'Beck''s 0,5l', '41001318', 100, 1, 500, 0, '4.9', '', ''),
(13, 'Almdudler 1,0l', '9015160402381', 166, 1, 1000, 0, '0.0', '', ''),
(14, 'Balisto Grün 37g', '5000159418546', 40, 1, NULL, 0, '0.0', '', ''),
(15, 'Snickers 57g', '5000159407397', 40, 1, NULL, 0, '0.0', '', ''),

-- --------------------------------------------------------

--
-- Table structure for table `transaction`
--

CREATE TABLE IF NOT EXISTS `transaction` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `product_id` int(11) DEFAULT NULL,
  `user_id` int(11) NOT NULL,
  `transaction_id` int(11) DEFAULT NULL COMMENT 'für storno',
  `type` enum('in','out','buy','move_in','move_out','cancel') NOT NULL,
  `price` int(11) NOT NULL,
  `date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `origin` varchar(255) NOT NULL COMMENT 'irc/console/mail/etc... (per app)',
  PRIMARY KEY (`id`),
  KEY `product_id` (`product_id`),
  KEY `user_id` (`user_id`),
  KEY `transaction_id` (`transaction_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=76 ;

--
-- Dumping data for table `transaction`
--

INSERT INTO `transaction` (`id`, `product_id`, `user_id`, `transaction_id`, `type`, `price`, `date`, `origin`) VALUES

-- --------------------------------------------------------

--
-- Table structure for table `user`
--

CREATE TABLE IF NOT EXISTS `user` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `mail` varchar(255) DEFAULT NULL,
  `gaf_acc` varchar(255) DEFAULT NULL,
  `irc` varchar(255) DEFAULT NULL,
  `total` int(11) DEFAULT '0' COMMENT 'cache',
  `note` text NOT NULL,
  `enabled` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=13 ;

--
-- Dumping data for table `user`
--

INSERT INTO `user` (`id`, `name`, `mail`, `gaf_acc`, `irc`, `total`, `note`, `enabled`) VALUES
(9, 'Dau', 'dau@localhost', NULL, NULL, 0, '', 1);

--
-- Constraints for dumped tables
--

--
-- Constraints for table `transaction`
--
ALTER TABLE `transaction`
  ADD CONSTRAINT `transaction_ibfk_1` FOREIGN KEY (`product_id`) REFERENCES `product` (`id`),
  ADD CONSTRAINT `transaction_ibfk_2` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`),
  ADD CONSTRAINT `transaction_ibfk_3` FOREIGN KEY (`transaction_id`) REFERENCES `transaction` (`id`);
