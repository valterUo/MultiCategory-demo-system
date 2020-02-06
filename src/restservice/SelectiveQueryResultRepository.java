package restservice;

import org.springframework.data.jpa.repository.JpaRepository;

interface SelectiveQueryResultRepository extends JpaRepository<SelectiveQueryResult, Long> {

}